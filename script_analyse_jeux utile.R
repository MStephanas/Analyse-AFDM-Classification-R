# =============================================================================
# MÉMOIRE - Pratiques de jeux de hasard à l'Université de Parakou
# Script de préparation des données pour analyse avec gtsummary
# ENSPD - Année académique 2025-2026
# =============================================================================
library(flextable)
library(officer)
library(haven)
library(dplyr)
library(labelled)
library(gtsummary)
library(factoextra)
library(ggplot2)
library(FactoMineR)
library(Factoshiny, lib.loc = "C:/Program Files/R/R-4.5.2/library")
library(rvg)
library(ggdendro)
library(dendextend)
# ============================
# 1. CHARGEMENT DE LA BASE
# ============================

base <- read_dta("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/questionnaire_enq_2025_2026_final-1.dta")

# ==================================
# 2. SÉLECTION DES VARIABLES UTILES
# ==================================
vars_retenues <- c(
  "ID_QUEST",
  "DEPARTEMENT", "DEPARTEMENT_DE_SOCIALISATION", "MILIEU_DE_SOCIALISATION",
  "Q102", "Q103", "Q104", "Q106", "Q107", "Q108", "Q109", "Q207",
  "Q401", "Q402", "Q403", "Q403A", "Q403B",
  "Q404REPEATING_1", "Q404REPEATING_2",
  "Q405REPEATING_1", "Q405REPEATING_2", "Q405REPEATING_3",
  "Q405REPEATING_4", "Q405REPEATING_5", "Q405REPEATING_6",
  "Q405REPEATING_8", "Q405REPEATING_9",
  "Q406REPEATING_1", "Q406REPEATING_2", "Q406REPEATING_3",
  "Q406REPEATING_4", "Q406REPEATING_5",
  "Q407REPEATING_1", "Q407REPEATING_2", "Q407REPEATING_3", "Q407REPEATING_4",
  "Q408", "Q410",
  "Q411REPEATING_1", "Q411REPEATING_2", "Q411REPEATING_3",
  "Q413REPEATING_1", "Q413REPEATING_2", "Q413REPEATING_3", "Q413REPEATING_4",
  "Q413REPEATING_5", "Q413REPEATING_6", "Q413REPEATING_7", "Q413REPEATING_8",
  "Q413REPEATING_9", "Q413REPEATING_11",
  "Q4014"
)

base_analyse <- base %>% select(all_of(vars_retenues))

# =============================================================================
# 3. CONVERSION haven_labelled → FACTEUR 
#    À faire AVANT le renommage et les filtres
# =============================================================================

base_analyse <- base_analyse %>%
  mutate(across(where(is.labelled), as_factor))

# =============================================================================
# 4. RENOMMAGE DES VARIABLES
# =============================================================================

base_analyse <- base_analyse %>%
  rename(
    id                        = ID_QUEST,
    departement               = DEPARTEMENT,
    dept_socialisation        = DEPARTEMENT_DE_SOCIALISATION,
    milieu_socialisation      = MILIEU_DE_SOCIALISATION,
    
    sexe                      = Q102,
    age                       = Q103,
    situation_matrimoniale    = Q104,
    religion                  = Q106,
    ethnie                    = Q107,
    entite                    = Q108,
    annee_etude               = Q109,
    revenu_mensuel            = Q207,
    
    connait_jeux              = Q401,
    deja_joue                 = Q402,
    joue_12mois               = Q403,
    mode_participation        = Q403A,
    age_debut_jeu             = Q403B,
    
    joue_en_ligne             = Q404REPEATING_1,
    joue_physique             = Q404REPEATING_2,
    
    plateforme_betclic        = Q405REPEATING_1,
    plateforme_1xbet          = Q405REPEATING_2,
    plateforme_bwin           = Q405REPEATING_3,
    plateforme_betpawa        = Q405REPEATING_4,
    plateforme_1win           = Q405REPEATING_5,
    plateforme_melbet         = Q405REPEATING_6,
    plateforme_lnb            = Q405REPEATING_8,
    plateforme_appli_mobile   = Q405REPEATING_9,
    
    jeu_ligne_casino          = Q406REPEATING_1,
    jeu_ligne_paris_sportifs  = Q406REPEATING_2,
    jeu_ligne_cartes          = Q406REPEATING_3,
    jeu_ligne_crash           = Q406REPEATING_4,
    jeu_ligne_virtuels        = Q406REPEATING_5,
    
    jeu_phys_machines_sous    = Q407REPEATING_1,
    jeu_phys_ludo             = Q407REPEATING_2,
    jeu_phys_dame             = Q407REPEATING_3,
    jeu_phys_domino           = Q407REPEATING_4,
    
    nb_sessions_ligne_30j     = Q408,
    montant_mise_mensuel      = Q410,
    
    finance_revenus_perso     = Q411REPEATING_1,
    finance_soutien_famille   = Q411REPEATING_2,
    finance_emprunts          = Q411REPEATING_3,
    
    impact_ameliore_scolaire  = Q413REPEATING_1,
    impact_reduit_stress      = Q413REPEATING_2,
    impact_bien_aise          = Q413REPEATING_3,
    impact_estime_soi_plus    = Q413REPEATING_4,
    impact_deteriore_scolaire = Q413REPEATING_5,
    impact_augmente_stress    = Q413REPEATING_6,
    impact_mal_aise           = Q413REPEATING_7,
    impact_estime_soi_moins   = Q413REPEATING_8,
    impact_conflits           = Q413REPEATING_9,
    impact_aucun              = Q413REPEATING_11,
    
    pertes_impact_finances    = Q4014
  )

# =============================================================================
# 5. CONSTRUCTION DE LA VARIABLE MODE DE JEU
#    joue_en_ligne et joue_physique sont numériques (1/0/NA)
# =============================================================================

base_analyse <- base_analyse %>%
  mutate(
    # Convertir en numérique pour la comparaison (après as_factor, elles restent num)
    joue_en_ligne_n = as.numeric(as.character(joue_en_ligne)),
    joue_physique_n = as.numeric(as.character(joue_physique)),
    
    mode_jeu = case_when(
      joue_en_ligne_n == 1 & joue_physique_n == 1 ~ "Hybride",
      joue_en_ligne_n == 1 & joue_physique_n == 0 ~ "En ligne uniquement",
      joue_en_ligne_n == 0 & joue_physique_n == 1 ~ "En point physique uniquement",
      TRUE                                         ~ NA_character_
    ),
    mode_jeu = factor(mode_jeu, levels = c(
      "En ligne uniquement",
      "En point physique uniquement",
      "Hybride"
    ))
  ) %>%
  select(-joue_en_ligne_n, -joue_physique_n)  # Supprimer variables temporaires

# =============================================================================
# 6. AJOUT DES LABELS DE VARIABLES (pour gtsummary) gtsummary est le package qui permet de gérer les tableaux et plus avancer summary
# =============================================================================

base_analyse <- base_analyse %>%
  set_variable_labels(
    departement               = "Département",
    dept_socialisation        = "Département de socialisation",
    milieu_socialisation      = "Milieu de socialisation",
    
    sexe                      = "Sexe",
    age                       = "Âge (années)",
    situation_matrimoniale    = "Situation matrimoniale",
    religion                  = "Religion",
    ethnie                    = "Ethnie",
    entite                    = "Entité de l'Université de Parakou",
    annee_etude               = "Année d'étude",
    revenu_mensuel            = "Revenu mensuel moyen",
    
    connait_jeux              = "Connaissance des jeux de hasard",
    deja_joue                 = "A déjà pratiqué les jeux de hasard",
    joue_12mois               = "A joué au cours des 12 derniers mois",
    mode_participation        = "Mode de participation",
    age_debut_jeu             = "Âge de début de jeu (années)",
    mode_jeu                  = "Mode de jeu",
    
    joue_en_ligne             = "Joue en ligne",
    joue_physique             = "Joue en point physique",
    
    plateforme_betclic        = "Betclic",
    plateforme_1xbet          = "1xBet",
    plateforme_bwin           = "Bwin",
    plateforme_betpawa        = "Betpawa",
    plateforme_1win           = "1win",
    plateforme_melbet         = "Melbet",
    plateforme_lnb            = "LNB / LMB (plateforme locale)",
    plateforme_appli_mobile   = "Application mobile / Jeux mobiles",
    
    jeu_ligne_casino          = "Casino en ligne",
    jeu_ligne_paris_sportifs  = "Paris sportifs en ligne",
    jeu_ligne_cartes          = "Jeux de cartes en ligne",
    jeu_ligne_crash           = "Jeux de Crash (Aviator, JetX...)",
    jeu_ligne_virtuels        = "Jeux virtuels (roulette, poker...)",
    
    jeu_phys_machines_sous    = "Machines à sous",
    jeu_phys_ludo             = "Ludo",
    jeu_phys_dame             = "Dame",
    jeu_phys_domino           = "Domino",
    
    nb_sessions_ligne_30j     = "Nombre de sessions en ligne (30 derniers jours)",
    montant_mise_mensuel      = "Montant moyen misé par mois",
    
    finance_revenus_perso     = "Revenus personnels",
    finance_soutien_famille   = "Soutien familial",
    finance_emprunts          = "Emprunts",
    
    impact_ameliore_scolaire  = "Ameliore le rendement scolaire",
    impact_reduit_stress      = "Reduit le stress",
    impact_bien_aise          = "Se sentir a l'aise",
    impact_estime_soi_plus    = "Augmente l'estime de soi",
    impact_deteriore_scolaire = "Deteriore le rendement scolaire",
    impact_augmente_stress    = "Augmente le stress",
    impact_mal_aise           = "Se sentir mal a l'aise",
    impact_estime_soi_moins   = "Reduit l'estime de soi",
    impact_conflits           = "Conflits avec autrui",
    impact_aucun              = "Aucun effet",
    pertes_impact_finances    = "Pertes avec impact sur finances ou vie"
  )

# =============================================================================
# 7. CONSTITUTION DES SOUS-BASES D'ANALYSE
# =============================================================================

# Tous les enquêtés
base_tous <- base_analyse

base_tous <- base_tous %>%
  mutate(
    religion_recod = case_when(
      religion == "Christianisme" ~ "Christianisme",
      religion == "Islam" ~ "Islam",
      religion == "Traditionnelle / Endogène" ~ "Traditionnelle / Endogène",
      religion == "Aucun" ~ "Aucune",
      religion == "Thron" ~ "Traditionnelle / Endogène",  # Thron → Endogène
      religion == "Eckankar / Baha'ie" ~ "Autres",        # Eckankar/Baha'ie → Autres
      religion == "Autres" ~ "Autres",
      TRUE ~ "Autres"
    ),
    religion_recod = factor(religion_recod, 
                            levels = c("Christianisme", "Islam", 
                                       "Traditionnelle / Endogène", 
                                       "Aucune", "Autres")),
    annee_etude_recod = case_when(
      annee_etude == "1ère année" ~ "1ère année",
      annee_etude == "2ème année" ~ "2ème année",
      annee_etude == "3ème année" ~ "3ème année",
      annee_etude == "4ème année" ~ "4ème année",
      annee_etude %in% c("5ème année", "7ème année") ~ "5ème année et plus",
      TRUE ~ NA_character_
    ),
    annee_etude_recod = factor(annee_etude_recod, 
                               levels = c("1ère année", "2ème année", "3ème année", 
                                          "4ème année", "5ème année et plus"))
  ,
    age_classe = case_when(
      age >= 14 & age <= 19 ~ "14-19 ans",
      age >= 20 & age <= 24 ~ "20-24 ans",
      age >= 25 & age <= 29 ~ "25-29 ans",
      age >= 30 ~ "30 ans et plus",
      TRUE ~ NA_character_
    ),
    age_classe = factor(age_classe, 
                        levels = c("14-19 ans", "20-24 ans", 
                                   "25-29 ans", "30 ans et plus"))
  )

# Vérifier le résultat
table(base_tous$religion, base_tous$religion_recod, useNA = "ifany")
table(base_tous$annee_etude, base_tous$annee_etude_recod, useNA = "ifany")
table(base_tous$age_classe, useNA = "ifany")
#===========================================================
# Joueurs actifs des 12 derniers mois (n = 1 364)
base_joueurs <- base_analyse %>%
  filter(joue_12mois == "Oui")

# Joueurs avec mise d'argent :
# Les apostrophes dans les valeurs sont typographiques (')
# On utilise grepl() pour éviter tout problème d'encodage
base_joueurs_mise <- base_joueurs %>%
  filter(grepl("mise d", mode_participation, fixed = FALSE))

# Vérification des effectifs
cat("=== Vérification des effectifs ===\n")
cat("Base complète         :", nrow(base_tous), "observations\n")
cat("Joueurs 12 mois       :", nrow(base_joueurs), "observations\n")
cat("Joueurs avec mise     :", nrow(base_joueurs_mise), "observations\n")
cat("\nRépartition mode de jeu :\n")
print(table(base_joueurs_mise$mode_jeu, useNA = "ifany"))

base_joueurs_mise <- base_joueurs_mise %>%
  mutate(
    age_classe = case_when(
      age >= 14 & age <= 19 ~ "14-19 ans",
      age >= 20 & age <= 24 ~ "20-24 ans",
      age >= 25 & age <= 29 ~ "25-29 ans",
      age >= 30 ~ "30 ans et plus",
      TRUE ~ NA_character_
    ),
    age_classe = factor(age_classe, 
                        levels = c("14-19 ans", "20-24 ans", 
                                   "25-29 ans", "30 ans et plus")),
    annee_etude_recod = case_when(
      annee_etude == "1ère année" ~ "1ère année",
      annee_etude == "2ème année" ~ "2ème année",
      annee_etude == "3ème année" ~ "3ème année",
      annee_etude == "4ème année" ~ "4ème année",
      annee_etude %in% c("5ème année", "7") ~ "5ème année et plus",
      TRUE ~ NA_character_
    ),
    annee_etude_recod = factor(annee_etude_recod, 
                               levels = c("1ère année", "2ème année", "3ème année", 
                                          "4ème année", "5ème année et plus"))
  
  )

# =============================================================================
# 8. TABLEAUX GTSUMMARY
# 
# --- Tableau 1 : Profil sociodémographique ---
tbl1_age_classe <- base_tous %>%
  select(
    sexe, 
    age_classe,  # Remplacer age par age_classe
    annee_etude_recod, 
    entite, 
    religion_recod, 
    ethnie, 
    milieu_socialisation, 
    revenu_mensuel
  ) %>%
  tbl_summary(
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    label = list(
      sexe ~ "Sexe",
      age_classe ~ "Classe d'âge",
      annee_etude_recod ~ "Année d'étude",
      entite ~ "Entité de l'Université de Parakou",
      religion_recod ~ "Religion",
      ethnie ~ "Ethnie",
      milieu_socialisation ~ "Milieu de socialisation",
      revenu_mensuel ~ "Revenu mensuel moyen"
    )
  ) %>%
  add_n() %>%
  modify_header(
    label = "**Caractéristiques**",
    stat_0 = "**N = {N}**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "n (%)"
  ) %>%
  modify_caption("**Tableau 1 : Profil sociodémographique des étudiants enquêtés**") %>%
  bold_labels()

tbl1_age_classe

# --- Tableau 2 : Profil selon le mode de jeu ---
# =======================
# Préparer les données
tbl2_age_classe <- base_joueurs_mise %>%
  select(mode_jeu, sexe, age_classe, annee_etude_recod, entite, revenu_mensuel) %>%
  tbl_summary(
    by = mode_jeu,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      sexe ~ "Sexe",
      age_classe ~ "Classe d'âge",
      annee_etude_recod ~ "Année d'étude",
      entite ~ "Entité de l'Université de Parakou",
      revenu_mensuel ~ "Revenu mensuel moyen"
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(
    label = "**Caractéristiques**",
    stat_1 = "**En ligne uniquement**<br>N = {n}",
    stat_2 = "**En point physique uniquement**<br>N = {n}",
    stat_3 = "**Hybride**<br>N = {n}",
    stat_0 = "**Ensemble**<br>N = {N}"
  ) %>%
  modify_spanning_header(c(stat_1:stat_3) ~ "**Mode de jeu**") %>%
  modify_footnote(
    all_stat_cols() ~ "n (%)"
  ) %>%
  modify_caption("**Tableau 2 : Profil sociodémographique selon le mode de jeu**") %>%
  bold_labels() %>%
  modify_table_styling(
    columns = label,
    rows = label == "p-value",
    label = "**p-valeur**"
  )

tbl2_age_classe

# Sauvegarder (optionnel)
# tbl_mode_jeu_final %>%
#   as_gt() %>%
#   gt::gtsave("Tableau2_profil_mode_jeu.docx")

# Tableau 3 
tbl3_corrige <- base_joueurs_mise %>%
  select(
    # Jeux en ligne
    jeu_ligne_paris_sportifs, jeu_ligne_casino, jeu_ligne_crash,
    jeu_ligne_cartes, jeu_ligne_virtuels,
    # Jeux en point physique
    jeu_phys_machines_sous, jeu_phys_ludo, jeu_phys_dame, jeu_phys_domino
  ) %>%
  tbl_summary(
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      jeu_ligne_paris_sportifs ~ "Paris sportifs en ligne",
      jeu_ligne_casino ~ "Casino en ligne",
      jeu_ligne_crash ~ "Jeux de Crash (Aviator, JetX...)",
      jeu_ligne_cartes ~ "Jeux de cartes en ligne",
      jeu_ligne_virtuels ~ "Jeux virtuels (roulette, poker...)",
      jeu_phys_machines_sous ~ "Machines à sous",
      jeu_phys_ludo ~ "Ludo",
      jeu_phys_dame ~ "Dame",
      jeu_phys_domino ~ "Domino"
    )
  ) %>%
  modify_header(
    label = "**Caractéristiques**",
    stat_0 = "**N = {N}**"
  ) %>%
  modify_caption("**Tableau 3 : Types de jeux pratiqués (en ligne et en point physique)**") %>%
  bold_labels()

print(tbl3_corrige)

# --- Tableau 4 : Comportement financier selon le mode de jeu ---
# Tableau 4 
tbl4_corrige <- base_joueurs_mise %>%
  filter(mode_jeu %in% c("En ligne uniquement", 
                         "En point physique uniquement", 
                         "Hybride")) %>%
  select(mode_jeu, 
         montant_mise_mensuel,
         finance_revenus_perso, 
         finance_soutien_famille, 
         finance_emprunts,
         pertes_impact_finances) %>%
  tbl_summary(
    by = mode_jeu,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      montant_mise_mensuel ~ "Montant moyen misé par mois",
      finance_revenus_perso ~ "Revenus personnels",
      finance_soutien_famille ~ "Soutien familial",
      finance_emprunts ~ "Emprunts",
      pertes_impact_finances ~ "Pertes avec impact sur finances ou vie"
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(test = all_categorical() ~ "fisher.test", 
        pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Caractéristiques**",
    stat_1 = "**En ligne uniquement**<br>N = {n}",
    stat_2 = "**En point physique uniquement**<br>N = {n}",
    stat_3 = "**Hybride**<br>N = {n}",
    stat_0 = "**Ensemble**<br>N = {N}"
  ) %>%
  modify_spanning_header(c(stat_1:stat_3) ~ "**Mode de jeu**") %>%
  modify_caption("**Tableau 4 : Comportement financier selon le mode de jeu**") %>%
  bold_labels() %>%
  # Traduction
  modify_table_styling(
    columns = label,
    rows = label == "p-value",
    label = "**p-valeur**"
  )

print(tbl4_corrige)
# --- Tableau 5 : Impacts des jeux selon le mode de jeu  ---
tbl_impacts <- base_joueurs_mise %>%
  select(mode_jeu,
         impact_deteriore_scolaire, impact_augmente_stress,
         impact_conflits, impact_mal_aise, impact_estime_soi_moins,
         impact_ameliore_scolaire, impact_reduit_stress, impact_aucun) %>%
  tbl_summary(
    by      = mode_jeu,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels() %>%
  modify_caption("**Tableau 5 : Impacts des jeux selon le mode de jeu**")

tbl_impacts

#Analyse Factorielle muktiple 
# =============================================================================
# PRÉPARATION DE LA BASE POUR AFCM ET CAH
# =============================================================================

# 1. Créer une base spécifique pour les joueurs avec mise
base_afcm <- base_joueurs_mise

# 2. Sélectionner uniquement les variables nécessaires
vars_afcm <- c(
  # Variables actives (types de jeux)
  "jeu_ligne_casino", "jeu_ligne_paris_sportifs", "jeu_ligne_cartes",
  "jeu_ligne_crash", "jeu_ligne_virtuels",
  "jeu_phys_machines_sous", "jeu_phys_ludo", "jeu_phys_dame", "jeu_phys_domino",
  
  # Variables illustratives
  "sexe", "age_classe", "situation_matrimoniale", "religion", "ethnie",
  "entite", "annee_etude_recod", "departement", "milieu_socialisation",
  "revenu_mensuel", "nb_sessions_ligne_30j", "montant_mise_mensuel",
  "age_debut_jeu", "mode_jeu"
)

base_afcm <- base_afcm %>% select(all_of(vars_afcm))

# 3. Vérifier la structure
glimpse(base_afcm)

# 4. S'assurer que les variables actives sont bien des facteurs
vars_actives <- c(
  "jeu_ligne_casino", "jeu_ligne_paris_sportifs", "jeu_ligne_cartes",
  "jeu_ligne_crash", "jeu_ligne_virtuels",
  "jeu_phys_machines_sous", "jeu_phys_ludo", "jeu_phys_dame", "jeu_phys_domino"
)

base_afcm <- base_afcm %>%
  mutate(across(all_of(vars_actives), as.factor))

##Lancement de l'AFCM
base_afcm <- base_afcm %>%
  mutate(
    nb_sessions_ligne_30j = as.numeric(as.character(nb_sessions_ligne_30j)),
    age_debut_jeu = as.numeric(as.character(age_debut_jeu))
  )

# 4. Gestion des NA (suppression des lignes avec NA)
base_afcm <- base_afcm %>%
  filter(!is.na(nb_sessions_ligne_30j) & 
           !is.na(age_debut_jeu))

# 5. Conversion des variables actives en facteurs
vars_actives <- c(
  "jeu_ligne_casino", "jeu_ligne_paris_sportifs", "jeu_ligne_cartes",
  "jeu_ligne_crash", "jeu_ligne_virtuels",
  "jeu_phys_machines_sous", "jeu_phys_ludo", "jeu_phys_dame", "jeu_phys_domino"
)

base_afcm <- base_afcm %>%
  mutate(across(all_of(vars_actives), as.factor))

# 6. Lancement de l'AFCM
res_afcm <- MCA(
  base_afcm,
  ncp = 5,
  quali.sup = c(
    "sexe", "situation_matrimoniale", "religion", "ethnie",
    "entite", " annee_etude_recod", "departement", "milieu_socialisation",
    "mode_jeu", "revenu_mensuel","montant_mise_mensuel", "age_classe"
  ),
  quanti.sup = c(
     "nb_sessions_ligne_30j", "age_debut_jeu"
  ),
  graph = TRUE
)

# Extraire et préparer les données (inchangé)
var_coords <- as.data.frame(res_afcm$var$coord)
var_coords$variable <- rownames(var_coords)
var_coords$type <- "Active"

quanti_coords <- if(!is.null(res_afcm$quanti.sup)) {
  df <- as.data.frame(res_afcm$quanti.sup$coord)
  df$variable <- rownames(df)
  df$type <- "Quantitative sup"
  df
} else { NULL }

quali_coords <- if(!is.null(res_afcm$quali.sup)) {
  df <- as.data.frame(res_afcm$quali.sup$coord)
  df$variable <- rownames(df)
  df$type <- "Qualitative sup"
  df
} else { NULL }

all_coords <- rbind(var_coords, quanti_coords, quali_coords)
colnames(all_coords)[1:2] <- c("Dim1", "Dim2")
all_coords <- all_coords %>% filter(!is.na(Dim1) & !is.na(Dim2))
#================
#Obtention des contributions:
#===============
# Fonction pour obtenir les meilleures contributions par axe
meilleures_contrib <- function(res_afcm, axe = 1, n = 5) {
  contrib <- as.data.frame(res_afcm$var$contrib)
  contrib <- contrib[order(contrib[, axe], decreasing = TRUE), ]
  top <- head(contrib[, axe], n)
  noms <- rownames(contrib)[1:n]
  result <- data.frame(
    Variable = noms,
    Contribution = round(top, 2)
  )
  return(result)
}

# Meilleures contributions à l'axe 1
print("=== 5 MEILLEURES CONTRIBUTIONS À L'AXE 1 ===")
print(meilleures_contrib(res_afcm, axe = 1, n = 5))

# Meilleures contributions à l'axe 2
print("=== 5 MEILLEURES CONTRIBUTIONS À L'AXE 2 ===")
print(meilleures_contrib(res_afcm, axe = 2, n = 5))
#=============================================

###obtention du screeplot))))))))))))
p2 <- fviz_eig(res_afcm,
               addlabels = TRUE,
               barfill = "#E69F00",
               barcolor = "#E69F00",
               linecolor = "#56B4E9",
               ncp = 10,
               ylim = c(0, 30),
               main = "Screeplot - Variance expliquée par dimension",
               xlab = "Dimensions",
               ylab = "Pourcentage de variance expliquée") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p2)

# === Obtention du graphique de L'AFCM avec ggplot codée manuellement =================================
#==============================================================

p3_ameliore <- ggplot(all_coords, aes(x = Dim1, y = Dim2)) +
  # Points beaucoup plus petits
  geom_point(aes(color = type), size = 0.8, alpha = 0.4) +
  # Étiquettes avec ajustement
  geom_text(aes(label = variable, color = type), 
            hjust = -0.1, 
            vjust = 0, 
            size = 4,          # Taille du texte légèrement réduite
            check_overlap = TRUE) +
  # Lignes des axes
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  # Couleurs plus douces
  scale_color_manual(values = c("Active" = "#E69F00", 
                                "Quantitative sup" = "#56B4E9", 
                                "Qualitative sup" = "#009E73")) +
  # Limites pour éviter que les étiquettes soient coupées
  xlim(min(all_coords$Dim1) - 0.2, max(all_coords$Dim1) + 0.2) +
  ylim(min(all_coords$Dim2) - 0.2, max(all_coords$Dim2) + 0.2) +
  # Labels
  labs(title = "Figure : Projection des variables sur les axes 1 et 2",
       x = paste0("Dimension 1 (", round(res_afcm$eig[1,2], 1), "%)"),
       y = paste0("Dimension 2 (", round(res_afcm$eig[2,2], 1), "%)"),
       color = "Type de variable") +
  # Thème propre
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Afficher
print(p3_ameliore)

# Sauvegarder en haute résolution
ggsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/graphique_afcm_amelioreplus.png", 
       plot = p3_ameliore, 
       width = 12, 
       height = 8, 
       dpi = 300,
       bg = "white")

ggsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/graphique_screeplot.png", 
       plot = p2, 
       width = 10, 
       height = 6, 
       dpi = 300,
       bg = "white")

tbl1_age_classe %>% 
  as_gt() %>% 
  gt::gtsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau1 .docx")

tbl2_age_classe %>% 
  as_gt() %>% 
  gt::gtsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau2 .docx")

tbl3_corrige %>% 
  as_gt() %>% 
  gt::gtsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau3 .docx")

tbl4_corrige %>% 
  as_gt() %>% 
  gt::gtsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau4 .docx")

#======================================================================
##Tableau avec FLEXTABLE :
# Conversion du tableau2 gtsummary vers flextable
#======================================================================
ft <- tbl2_age_classe %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit") %>%   # Ajuste automatiquement la largeur
  flextable::theme_booktabs() %>%                           # Thème 
  flextable::fontsize(size = 10, part = "all") %>%          # Taille de police uniforme
  flextable::bold(part = "header") %>%                      # En-têtes en gras
  flextable::align(align = "center", part = "all")          # Centrer le contenu

# Export vers Word
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("Tableau 2 : Profil sociodémographique selon le mode de jeu", style = "heading 2")

print(doc, target = "C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau2.docx")

#Export du tableau 4 vers word version flextable )))

# Conversion du tableau gtsummary vers flextable
ft4 <- tbl4_corrige %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit") %>%   # Ajuste automatiquement la largeur
  flextable::theme_booktabs() %>%                           # Thème élégant
  flextable::fontsize(size = 10, part = "all") %>%          # Taille de police uniforme
  flextable::bold(part = "header") %>%                      # En-têtes en gras
  flextable::align(align = "center", part = "all")          # Centrer le contenu

# Export vers Word
doc4 <- read_docx() %>%
  body_add_flextable(ft4) %>%
  body_add_par("Tableau 4 : Comportement financier selon le mode de jeu", style = "heading 2")

print(doc4, target = "C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/Tableau4flex.docx")

#======Sauvegarde de screeplot dans word 
doc <- read_docx() %>%
  body_add_vg(
    code = print(p3_ameliore),   # le plot ggplot
    type = "plot"
  )

print(doc, target = "p2_word_vector.docx")





#####Prép&rtion de la CAH*************************===============
# 1. Récupérer les coordonnées des individus depuis l'AFCM
ind_coords <- as.data.frame(res_afcm$ind$coord)

# 2. Garder seulement les axes importants (par exemple les 5 premiers)
#    qui cumulent un pourcentage significatif de variance
ind_coords <- ind_coords[, 1:5]  # Ajustez le nombre 

###LANCEMENT DE LA CAH======================================
# 1. CAH avec la méthode de Ward
res_cah <- HCPC(
  res_afcm,           # Utiliser directement les résultats de l'AFCM
  nb.clust = -1,      # -1 = laisser l'algorithme choisir le nombre de classes
  metric = "euclidean",
  method = "ward",
  graph = FALSE
)

# 2. Visualiser le dendrogramme
fviz_dend(res_cah, 
          show_labels = FALSE,
          rect = TRUE, 
          rect_fill = TRUE,
          main = "Dendrogramme de la classification des joueurs")

# ============================
# CARACTÉRISATION DES CLASSES
# ===========================

# 1. Variables les plus caractéristiques de chaque classe
print("=== VARIABLES CARACTÉRISANT CHAQUE CLASSE ===")
print(res_cah$desc.var)

# 2. Axes les plus associés à chaque classe
print("=== AXES CARACTÉRISANT CHAQUE CLASSE ===")
print(res_cah$desc.axes)

# 3. Individus typiques de chaque classe
print("=== INDIVIDUS TYPIQUES ===")
print(res_cah$desc.ind)

# =============================================================================
# AFFECTATION DES CLASSES
# 
# 1. Récupérer les indices des individus utilisés dans l'AFCM/CAH
indices_classifies <- as.numeric(rownames(res_afcm$ind$coord))

# 2. Créer une nouvelle colonne classe avec NA par défaut
base_joueurs_mise$classe <- NA

# 3. Affecter les classes uniquement aux individus classifiés
base_joueurs_mise$classe[indices_classifies] <- res_cah$data.clust$clust

# 4. Vérifier la répartition
print("=== RÉPARTITION DES CLASSES ===")
print(table(base_joueurs_mise$classe, useNA = "ifany"))

# 5. Créer une version factorisée pour les tableaux
base_joueurs_mise$classe_factor <- factor(base_joueurs_mise$classe,
                                          levels = 1:3,
                                          labels = c("Classe 1", "Classe 2", "Classe 3"))


# =============================================================================
# TABLEAU DESCRIPTIF DES CLASSES s
# =============================================================================

# Utiliser seulement les individus classifiés pour le tableau
base_classes <- base_joueurs_mise[!is.na(base_joueurs_mise$classe), ]

cat("=== INDIVIDUS CLASSIFIÉS ===\n")
cat("Nombre :", nrow(base_classes), "\n")

# Tableau descriptif des classes
tbl_classes <- base_classes %>%
  select(classe_factor, sexe, age_classe, annee_etude_recod, entite, revenu_mensuel,
         montant_mise_mensuel, finance_emprunts) %>%
  tbl_summary(
    by = classe_factor,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      sexe ~ "Sexe",
      age_classe ~ "Classe d'âge",
      annee_etude_recod ~ "Année d'étude",
      entite ~ "Entité",
      revenu_mensuel ~ "Revenu",
      montant_mise_mensuel ~ "Montant misé",
      finance_emprunts ~ "Emprunts"
    )
  ) %>%
  add_overall() %>%
  modify_header(
    label = "**Caractéristiques**",
    all_stat_cols() ~ "**{level}**<br>N = {n}"
  ) %>%
  modify_caption("**Tableau : Profil sociodémographique par classe**") %>%
  bold_labels()

print(tbl_classes)
#======================

# =============================================================================
# RÉCAPITULATIF DES CLASSES 
# =============================================================================

# Calculer les pourcentages
classe_counts <- table(base_classes$classe)
classe_pct <- round(100 * classe_counts / sum(classe_counts), 1)

print("=== POURCENTAGES PAR CLASSE ===")
for(i in 1:length(classe_pct)) {
  cat("Classe", i, ":", classe_counts[i], "individus soit", classe_pct[i], "%\n")
}

# Tableau récapitulatif des classes
classes_profiles <- base_classes %>%
  group_by(classe) %>%
  summarise(
    effectif = n(),
    pourcentage = round(n() / nrow(base_classes) * 100, 1),
    sexe_ratio = paste0(round(sum(sexe == "Masculin", na.rm = TRUE) / n() * 100, 1), "% hommes"),
    age_dominant = names(sort(table(age_classe), decreasing = TRUE))[1],
    entite_dominante = names(sort(table(entite), decreasing = TRUE))[1],
    mise_dominante = names(sort(table(montant_mise_mensuel), decreasing = TRUE))[1],
    emprunts = paste0(round(sum(finance_emprunts == "Oui", na.rm = TRUE) / n() * 100, 1), "%")
  )

print("=== PROFIL RÉCAPITULATIF DES CLASSES ===")
print(classes_profiles)

# Nommer les classes selon leurs caractéristiques
base_classes <- base_classes %>%
  mutate(
    profil = case_when(
      classe == 1 ~ "Parieurs connectés",
      classe == 2 ~ "Joueurs traditionnels",
      classe == 3 ~ "Hybrides",
      TRUE ~ as.character(classe)
    )
  )

print("=== DISTRIBUTION DES PROFILS ===")
print(table(base_classes$profil))



# =============================================================================
# DENDOGRAMME version qui est concervé dans le rapport


# 2. Reconstruire les objets si nécessaire
if(!exists("classe_counts")) {
  # Récupérer les indices des individus classifiés
  indices_classifies <- as.numeric(rownames(res_afcm$ind$coord))
  
  # Affecter les classes
  base_joueurs_mise$classe <- NA
  base_joueurs_mise$classe[indices_classifies] <- res_cah$data.clust$clust
  
  # Créer base_classes
  base_classes <- base_joueurs_mise[!is.na(base_joueurs_mise$classe), ]
  
  # Calculer les pourcentages
  classe_counts <- table(base_classes$classe)
  classe_pct <- round(100 * classe_counts / sum(classe_counts), 1)
  
  cat("=== INDIVIDUS CLASSIFIÉS ===\n")
  cat("Nombre :", nrow(base_classes), "\n")
  print(classe_counts)
  print(paste("Pourcentages :", paste(classe_pct, collapse = "%, "), "%"))
}

# 3. Créer le dendrogramme
hclust_obj <- res_cah$call$t$tree
k <- length(classe_counts)
hauteur_coupure <- hclust_obj$height[length(hclust_obj$height) - (k - 1)]

dend <- as.dendrogram(hclust_obj)
dend_data <- dendro_data(dend)

segments_horiz <- subset(dend_data$segments, y == yend & x != xend)
segments_horiz <- segments_horiz[order(segments_horiz$y, decreasing = TRUE), ]
segments_horiz <- head(segments_horiz, 7)

leaf_order <- order.dendrogram(dend)
cluster_assignments <- cutree(dend, k = k)[leaf_order]
x_positions <- tapply(seq_along(cluster_assignments), cluster_assignments, median)

# 4. Graphique de  la dendogramme 
p_dend_final <- ggplot() +
  geom_segment(data = dend_data$segments, 
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray30", linewidth = 0.15) +
  geom_hline(yintercept = hauteur_coupure, 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", 
           x = max(dend_data$segments$x) * 0.7, 
           y = hauteur_coupure + 0.03,
           label = paste("Coupure à", round(hauteur_coupure, 3), "pour", k, "classes"),
           size = 3, color = "red", hjust = 0) +
  geom_text(data = segments_horiz,
            aes(x = (x + xend)/2, y = y + 0.02,
                label = paste0(round(y * 100, 1), "%")),
            size = 3, fontface = "bold", color = "black") +
  geom_text(data = data.frame(x = x_positions, y = -0.1, label = paste0(classe_pct, "%")),
            aes(x = x, y = y, label = label),
            size = 6, fontface = "bold", color = "black") +
  geom_text(data = data.frame(x = x_positions, y = -0.18, label = 7:(7+k-1)),
            aes(x = x, y = y, label = label),
            size = 3, color = "gray60") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.margin = margin(20, 20, 40, 20)) +
  labs(title = "Dendrogramme de la classification hiérarchique")

# 5. Afficher et sauvegarder
print(p_dend_final)
ggsave("C:/Users/AKPAKI Kamila/Desktop/Analyse enquet/pdf Et png/dendrogramme_final.png", 
       plot = p_dend_final, width = 10, height = 6, dpi = 600, bg = "white")

cat(" Dendrogramme créé avec succès !\n")


