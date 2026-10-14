"""Thresholds for aligned current-map calibration scores; no model fitting."""
import math


def quantile_type7(values, probability=0.1):
    values = sorted(float(x) for x in values if x is not None and math.isfinite(float(x)))
    if not values or not 0 <= probability <= 1:
        raise ValueError('Finite scores and a probability in [0,1] are required')
    position = (len(values) - 1) * probability
    lower = int(position)
    upper = min(lower + 1, len(values) - 1)
    return values[lower] + (position - lower) * (values[upper] - values[lower])


def maximum_tss(scores, labels):
    """Exact sweep for score >= cutoff; smallest cutoff wins ties."""
    if len(scores) != len(labels):
        raise ValueError('Scores and labels must align')
    pairs = []
    for score, label in zip(scores, labels):
        if label not in (0, 1):
            raise ValueError('Explicit binary labels are required')
        if score is not None and math.isfinite(float(score)):
            pairs.append((float(score), label))
    positives = sum(label for _, label in pairs)
    negatives = len(pairs) - positives
    if not positives or not negatives:
        raise ValueError('Finite presence and background scores are required')
    pairs.sort()
    tp, fp = positives, negatives
    best = None
    index = 0
    while index < len(pairs):
        cutoff = pairs[index][0]
        value = tp / positives - fp / negatives
        if best is None or value > best['tss'] + 1e-12:
            best = dict(cutoff=cutoff, tss=value,
                        sensitivity=tp / positives, specificity=1 - fp / negatives)
        while index < len(pairs) and pairs[index][0] == cutoff:
            if pairs[index][1]:
                tp -= 1
            else:
                fp -= 1
            index += 1
    # All-negative classification is attainable above the highest observed score.
    if 0 > best['tss'] + 1e-12:
        best = dict(cutoff=math.nextafter(pairs[-1][0], math.inf), tss=0.0,
                    sensitivity=0.0, specificity=1.0)
    return best


def binarize(values, cutoff):
    if not math.isfinite(cutoff):
        raise ValueError('A finite cutoff is required')
    return [None if x is None or math.isnan(float(x)) else int(float(x) >= cutoff)
            for x in values]
