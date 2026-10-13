# Geographic sensitivity on the verified complete city sample.
run_geographic <- function(frames, forms, reference, roster = NULL, only_regions = NULL, suffix = "") {
  summaries <- scans <- skips <- list()
  regions <- if(is.null(only_regions)) unique(reference$continent) else only_regions
  for (response in names(frames)) for (region in regions) {
    f <- frames[[response]]; f <- f[f$continent != region, ]
    for (model in names(forms)) {
      key <- paste(response, region, model, sep="_")
      if (nrow(f) < 50L || sum(frames[[response]]$continent==region)<3L) {
        skips[[key]] <- data.frame(response=response,excluded_continent=region,model=model,n_remaining=nrow(f),n_excluded=sum(frames[[response]]$continent==region),reason="fewer than 50 remaining or fewer than 3 excluded cities")
        next
      }
      m <- gam(forms[[model]], data=f, method="REML")
      summaries[[key]] <- summary_row(m,response,model,paste0("exclude_",region))
      z <- scan_model(m,model,reference,f,response); z$excluded_continent <- region; scans[[key]] <- z
    }
  }
  write_table(do.call(rbind,summaries),"sampling",paste0("leave_continent_out_models",suffix,".csv"))
  write_table(do.call(rbind,scans),"sampling",paste0("leave_continent_out_endpoint_scans",suffix,".csv"))
  if(length(skips)) write_table(do.call(rbind,skips),"sampling",paste0("geographic_support_blocks",suffix,".csv"),"qc")
  status <- "BLOCKED: original 135 city pool not supplied"
  if (!is.null(roster)) {
    p <- openxlsx::read.xlsx(roster,sheet="Sengupta et al 2021_reclamation")
    p <- p[!is.na(p$City)&trimws(p$City)!="",]
    norm <- function(x) trimws(gsub("[^a-z0-9]+"," ",tolower(x),perl=TRUE))
    key <- norm(p$City); target <- norm(reference$city)
    if (nrow(p)==135L && !anyDuplicated(key) && all(target %in% key)) {
      p$retained <- key %in% target
      write_table(p,"sampling","original_roster_membership.csv")
      status <- "PARTIAL: original roster verified; excluded covariates and sampling effort absent"
    } else {
      write_table(data.frame(city=reference$city,matched=target%in%key),"sampling","roster_identity_check.csv","qc")
      status <- "BLOCKED: 135 roster membership is not unambiguous"
    }
  }
  write_table(data.frame(module="sampling_selection",status=status,geographic_sensitivity="TECHNICALLY_PASSED",sampling_effort="unavailable; not invented"),"sampling",paste0("module_status",suffix,".csv"),"qc")
  z <- do.call(rbind,scans)
  ggsave(out("sampling","figures",paste0("geographic_endpoint_scans",suffix,".png")),ggplot(z[z$model=="G0",],aes(city_area_km2,endpoint_difference,colour=excluded_continent))+geom_hline(yintercept=0,colour="grey50")+geom_line()+facet_wrap(~response,scales="free_y")+scale_x_log10()+labs(x="City area (km2)",y="Predicted endpoint difference",colour="Excluded continent")+theme_bw(),width=10,height=5,dpi=200)
}
