# function to read in all sheets in a workbook
filename <- 'RubellaModel.xlsx'
read_excel_allsheets <- function(filename, tibble = FALSE) {
  
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X){ print(X)
    openxlsx::read.xlsx(filename, sheet = X)})
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Log levels to determine info to be returned when logging
LEVEL <<-  list(
  'trace',
  'info',
  'error',
  'fatal',
  'warn',
  'debug'
)
names(LEVEL) <- toupper(LEVEL)
# https://stackoverflow.com/questions/7307987/logging-current-function-name
curfnfinder<-function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
                      retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
    currv<-sys.call(sys.parent(n=i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix<-gsub("function[ \t]*\\(.*", "do.call", prefix)
  if(length(prefix)==0)
  {
    return(retIfNone)
  }
  else if(retStack)
  {
    return(paste(rev(prefix), collapse = "|"))
  }
  else
  {
    retval<-as.character(unlist(prefix[1]))
    if(length(prefix) > 1)
    {
      retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
    }
    return(retval)
  }
}
makeLogger <- function(default_level='trace') {
  LOG <<- function(msg, level=default_level, .envir=parent.frame()) {
    FROM <- tryCatch(this.path::this.path(),error=function(e) '.')
    FUNC <- stringr::str_trim(curfnfinder(skipframes = 1))
    msg <- as.character(glue::glue(as.character(msg), .envir=.envir))
    PRE <- paste0('(',
                  crayon::italic(paste0(fs::path_rel(FROM), ':', FUNC)),
                  ') ')
    MSG <- paste0(PRE, msg)
    switch (tolower(level),
            'trace' = rlog::log_trace,
            'info'  = rlog::log_info,
            'error' = rlog::log_error,
            'fatal' = rlog::log_fatal,
            'warn'  = rlog::log_warn,
            'debug' = rlog::log_debug
    )(MSG)
  }
  LOG
}
setLogLevel <- function(level) {
  Sys.setenv(LOG_LEVEL=toupper(level))
}

# function to used to generate all the parameter names
getParmNames <- function(){
  'delta_D,theta_Ia_D,p_a_v_D,p_a_pv_D,p_a_D,p_s_D,p_s_pv_D,p_s_v_D,
    omega_D,p_nt_d_D,p_t_d_D,
    gamma_m_D,gamma_t_D,gamma_nt_D,phi_D,tau_D,
    eff_1_D,eff_2_D,eff_3_D,eff_b_D,eff_cb_D,eff_ab_D,eff_m_D,
    tau_1_D,tau_2_D,tau_3_D,tau_b_D,tau_cb_D,tau_ab_D,tau_mi_D,tau_mm_D,tau_vmm_D,
    mov_D,movm_D,
    deathprop,births,femProp,fertProp,
    p_mt_D,p_st_D,p_rep_clin_D,p_rep_death_D,
    whenDose_1,whenDose_2,whenDose_3,
    whenDose_b,whenDose_cb,whenDose_ab,whenDose_mb,
    whenDose_ib,whenDose_icb,whenDose_iab,whenDose_imb,
    ptrans_D,
    cov1yr,cov2yr,cov3yr,covbyr,covcbyr,covabyr,
    covm,contact,
    unit_cpd_ps,unit_cpd_b,unit_cpd_cb,unit_cpd_ab,unit_cpd_m,
    unit_cpd_ib,unit_cpd_icb,unit_cpd_iab,unit_cpd_im,
    outp_cost_D,inp_cost_D,strat_intro_cost,strat_intro_applyonce,
    del_hf_ps,del_hf_b,del_hf_cb,del_hf_ab,del_hf_m,
    del_hf_ib,del_hf_icb,del_hf_iab,del_hf_im,
    del_o_ps,del_o_b,del_o_cb,del_o_ab,del_o_m,
    del_o_ib,del_o_icb,del_o_iab,del_o_im,
    del_cpd_hf_ps,del_cpd_hf_b,del_cpd_hf_cb,del_cpd_hf_ab,del_cpd_hf_m,
    del_cpd_hf_ib,del_cpd_hf_icb,del_cpd_hf_iab,del_cpd_hf_im,
    del_cpd_o_ps,del_cpd_o_b,del_cpd_o_cb,del_cpd_o_ab,del_cpd_o_m,
    del_cpd_o_ib,del_cpd_o_icb,del_cpd_o_iab,del_cpd_o_im,
    del_pfin_ps,del_pfin_b,del_pfin_cb,del_pfin_ab,del_pfin_m,
    del_pfin_ib,del_pfin_icb,del_pfin_iab,del_pfin_im,
    unit_pfin_ps,unit_pfin_b,unit_pfin_cb,unit_pfin_ab,unit_pfin_m,
    unit_pfin_ib,unit_pfin_icb,unit_pfin_iab,unit_pfin_im,
    strat_intro_cost_pfin' %>%
    str_replace_all('\n','') %>%
    str_replace_all(' ','') %>%
    str_replace_all('\t','') %>%
    str_split_1(',')
}

# function to create the workbook using the .rds file
makeWorkbook <- function(fname='parameters/shinyInputs.xlsx') {
  result <- readRDS('shinyInputParameters_adolescent.rds')
  allNames <- getParmNames()
  
  nm <- allNames[[1]]
  xsing <- result[['delta_D']]
  xmat <- result[['cov2yr']]
  xvec <- result[['deathprop']]
  tbResult <- tibble(name = allNames, value=result[allNames]) %>%
    mutate(type = map_chr(name, function(nm) {
      case_when(length(result[[nm]]) <= 1 ~ 'single',
                is.null(dim(result[[nm]])) ~ paste0('vec_',length(result[[nm]])),
                .default = paste0('mat_',paste0(dim(result[[nm]]),collapse = 'x')))
    }))
  tbResultSummary <- tbResult %>%
    summarize(.by=type, names=paste0(name, collapse=','))
  
  workbookList <- list()
  ## "single"
  workbookList[['singles']] <- tbResult %>%
    filter(type=='single') %>%
    unnest(value, keep_empty=TRUE) %>%
    select(!type)
  
  ## "vec_43"
  workbookList[['vecTime']] <- tbResult %>%
    filter(type=='vec_43') %>%
    select(!type) %>%
    unnest_wider(value)
  
  ## "vec_56"
  workbookList[['vecAge']] <- tbResult %>%
    filter(type=='vec_56') %>%
    select(!type) %>%
    unnest_longer(value) %>%
    group_by(name) %>%
    group_map(~rowid_to_column(.x), .keep = T) %>%
    bind_rows() %>%
    pivot_wider(id_cols = rowid, names_from = name, values_from=value) %>%
    select(!rowid)
  
  ## "mat_56x56"
  workbookList[['contact']] <- tbResult %>%
    filter(type=='mat_56x56') %>%
    select(!type) %>%
    pull(value, name=name) %>%
    first()
  
  ## "mat_56x43"
  matList <- tbResult %>%
    filter(type=='mat_56x43') %>%
    select(!type) %>%
    pull(name=name)
  workbookList <- append(workbookList, matList)
  
  openxlsx::write.xlsx(workbookList, fname)
}

# function to retrieve the inputs from a saved workbook
getWorkbook <- function(fname='inputsBaseline_Diphtheria.xlsx') {
  workbookList <- read_excel_allsheets(glue::glue("parameters/{fname}"))
  sheet_names <- names(workbookList)
  
  ## singles
  result <- workbookList$singles %>% pull(value, name=name)
  
  # Yes, there's a better way to do this
  ## vec_time
  r1 <- workbookList$vecTime %>%
    pivot_longer(!name, names_to='year') %>%
    pivot_wider(names_from = name, values_from = value) %>%
    as.list()
  
  r1 <- names(r1) %>%
    setdiff('year') %>%
    set_names(.,.) %>%
    map(function(nm) {
      names(r1[[nm]]) <- r1$year
      r1[[nm]]
    })
  
  result <- append(result, r1)
  
  ## vec_age
  r1 <- as.list(workbookList$vecAge)
  result <- append(result, r1)
  ## contact
  contact <- read_excel(
    "parameters/inputsBaseline_Diphtheria.xlsx", 
    sheet = "contact", 
    col_names = paste0("V", 1:56)
  ) %>% as.matrix()
  colnames(contact) <- NULL
  dimnames(contact) <- NULL
  r1 <- list(contact=contact)
  result <- append(result, r1)
  
  ## mat_age_time
  r1 <- sheet_names %>%
    setdiff(c('singles','vecTime','vecAge','contact')) %>%
    set_names(.,.) %>%
    map(function(.x) {
      as.matrix(workbookList[[.x]])
    })
  result <- append(result, r1)
  
  ## all done
  result
}

test   = openxlsx::read.xlsx(filename, sheet = "Death")
