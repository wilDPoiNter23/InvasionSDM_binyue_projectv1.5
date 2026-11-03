// Export saved habitat comparisons as an editable supplementary workbook.
import fs from 'node:fs/promises';
import path from 'node:path';
import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';

const [inputPath, outputPath] = process.argv.slice(2);
if (!inputPath || !outputPath) throw new Error('Provide the saved table JSON and output workbook path.');
const runtimeRequire = createRequire(path.join(process.cwd(), 'spreadsheet-runtime.cjs'));
const { Workbook, SpreadsheetFile } = await import(
  pathToFileURL(runtimeRequire.resolve('@oai/artifact-tool')).href
);
const source = JSON.parse(await fs.readFile(inputPath, 'utf8'));
const workbook = Workbook.create();
const groupLabels = {
  FULL_814: 'Environment + human: primary',
  PAIRED_FULL: 'Environment + human: matched',
  PAIRED_ENV: 'Environment only: matched',
};
const responseLabels = {
  mean: 'Mean suitability (0-1)',
  fraction: 'Suitable fraction',
  area: 'Effective suitable area (km2)',
};
const headers = ['Fig. 4 panel', 'Prediction set', 'Spatial extent', 'Response',
  'Threshold', 'Estimate at 0.81 km2', 'Estimate at 23.73 km2',
  'Contrast (model scale)', '95% CI lower', '95% CI upper', 'P value'];

async function addSheet(name, records, title, includeNotes) {
  const sheet = workbook.worksheets.add(name);
  sheet.showGridLines = false;
  const end = 3 + records.length;
  sheet.getRange(`A1:K${end}`).format.font = { name: 'Arial', color: '#202529' };
  sheet.getRange('A1:K1').merge();
  sheet.getRange('A1').values = [[title]];
  sheet.getRange('A1:K1').format.font = { name: 'Arial', bold: true, color: '#202529' };
  sheet.getRange('A1:K1').format.rowHeight = 30;
  sheet.getRange('A2:K2').merge();
  sheet.getRange('A2').values = [[source.comparison]];
  sheet.getRange('A2:K2').format.rowHeight = 25;
  sheet.getRange('A3:K3').values = [headers];
  sheet.getRange('A3:K3').format = {
    fill: '#E8EDF0', font: { name: 'Arial', bold: true, color: '#202529' },
    wrapText: true, rowHeight: 42, verticalAlignment: 'center',
    borders: { bottom: { style: 'thin', color: '#AAB4BA' } },
  };
  const values = records.map(r => [r.figure_panel || '', groupLabels[r.group],
    r.region === 'CITY' ? 'Whole city, including reclaimed land' : 'Mapped reclaimed land',
    responseLabels[r.response], r.threshold === 'NONE' ? 'Not applicable' :
      r.threshold === 'T10' ? '10% omission' : 'Maximum TSS',
    r.low_fitted, r.high_fitted, r.difference, r.lower, r.upper, r.p_value]);
  sheet.getRange(`A4:K${end}`).values = values;
  sheet.getRange(`A4:E${end}`).format.wrapText = true;
  sheet.getRange(`A4:K${end}`).format.rowHeight = 42;
  sheet.getRange(`A4:K${end}`).format.verticalAlignment = 'center';
  sheet.getRange(`F4:G${end}`).setNumberFormat('0.000');
  sheet.getRange(`H4:J${end}`).setNumberFormat('0.0000');
  sheet.getRange(`K4:K${end}`).setNumberFormat('[<0.001]"<0.001";0.000');
  for (let i = 0; i < records.length; i++) {
    const row = i + 4;
    if (records[i].response === 'fraction') sheet.getRange(`F${row}:G${row}`).setNumberFormat('0.0%');
    if (records[i].response === 'area') sheet.getRange(`F${row}:G${row}`).setNumberFormat('#,##0.00');
    if (i % 2 === 1) sheet.getRange(`A${row}:K${row}`).format.fill = '#F6F8F9';
  }
  const widths = [11, 28, 31, 28, 18, 19, 19, 21, 16, 16, 16];
  widths.forEach((width, i) => {
    const col = String.fromCharCode(65 + i);
    sheet.getRange(`${col}1:${col}${end + 8}`).format.columnWidth = width;
  });
  sheet.freezePanes.freezeRows(3);
  let lastRow = end;
  {
    const notes = includeNotes ? source.notes : source.notes.slice(0, 3);
    for (let i = 0; i < notes.length; i++) {
      const row = end + 2 + i;
      sheet.getRange(`A${row}:K${row}`).merge();
      sheet.getRange(`A${row}`).values = [[notes[i]]];
      sheet.getRange(`A${row}:K${row}`).format = {
        wrapText: true, rowHeight: 32, font: { name: 'Arial', color: '#50585D' },
      };
      lastRow = row;
    }
  }
  return { name, lastRow };
}

const sheets = [];
sheets.push(await addSheet(source.figure_sheet_name, source.primary,
  source.figure_title, false));
const complete = [...source.complete].sort((a, b) => {
  const pa = a.figure_panel || 'z'; const pb = b.figure_panel || 'z';
  return pa.localeCompare(pb) || a.region.localeCompare(b.region) ||
    a.group.localeCompare(b.group) || a.threshold.localeCompare(b.threshold) ||
    a.response.localeCompare(b.response);
});
sheets.push(await addSheet('Supplementary_Table_18', complete, source.title, true));
workbook.recalculate();
console.log((await workbook.inspect({ kind: 'table', range: `${source.figure_sheet_name}!A3:K7`,
  include: 'values', tableMaxRows: 5, tableMaxCols: 11, maxChars: 2600 })).ndjson);
console.log((await workbook.inspect({ kind: 'match',
  searchTerm: '#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!|#SPILL!|#CALC!',
  options: { useRegex: true, maxResults: 20 }, maxChars: 1000 })).ndjson);
for (const item of sheets) {
  const preview = await workbook.render({ sheetName: item.name,
    range: `A1:K${item.lastRow}`, scale: 1, format: 'png' });
  await fs.writeFile(path.join(path.dirname(outputPath), `${item.name}_preview.png`),
    new Uint8Array(await preview.arrayBuffer()));
}
const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(outputPath);
console.log(`Saved ${source.primary.length} figure rows and ${source.complete.length} complete comparison rows.`);
