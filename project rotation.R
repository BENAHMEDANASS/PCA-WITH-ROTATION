install.packages("dplyr")
install.packages("psych")
install.packages("janitor")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("ggrepel")


# --- 1. CHARGEMENT DES BIBLIOTHEQUES ---

# Pour la manipulation de donnees
library(dplyr)
library(janitor)

# Pour l'analyse factorielle (EFA)
library(psych)

# Pour la visualisation
library(ggplot2)
library(factoextra)
library(ggrepel)


# --- 2. CHARGEMENT ET PREPARATION DES DONNEES ---
data_raw <- read.csv(file.choose())
data_clean <- clean_names(data_raw)

# Selectionner uniquement les colonnes de satisfaction pour notre analyse

satisfaction_vars <- c(
  "inflight_wifi_service", "departure_arrival_time_convenient",
  "ease_of_online_booking", "gate_location", "food_and_drink",
  "online_boarding", "seat_comfort", "inflight_entertainment",
  "on_board_service", "leg_room_service", "baggage_handling",
  "checkin_service", "inflight_service", "cleanliness"
)


# Creer notre tableau de donnees final pour l'EFA
efa_data <- data_clean %>%
  select(all_of(satisfaction_vars)) %>%
  
  # Les notes "0" signifient "non applicable", il faut les enlever
  filter(across(everything(), ~ . != 0)) %>%
  
  # Supprimer les lignes avec des valeurs manquantes
  na.omit()


# Afficher les premieres lignes pour verifier que tout va bien
 head(efa_data)

 # --- 3. VERIFICATION DES PREREQUIS ---
 
 
 # Test de KMO (Kaiser-Meyer-Olkin) -> on veut un resultat > 0.6
 
 kmo_result <- KMO(efa_data)
 
 print("Resultat du test KMO :")
 print(kmo_result)
 # Test de Bartlett -> on veut une p-value tres petite (< 0.05)
 bartlett_result <- cortest.bartlett(efa_data)
 print("Resultat du test de Bartlett :")
 print(bartlett_result)
 
 
 # --- 4. DETERMINER LE NOMBRE DE COMPOSANTES A RETENIR ---
 
 # Nous allons utiliser deux methodes en meme temps grace a un seul graphique.
 
 # --- 4.1. VISUALISATION DU CRITERE DU COUDE (SCREE PLOT) SEPAREMENT ---
 
 # On fait une ACP simple SANS rotation juste pour obtenir les valeurs propres.
 
 acp_simple <- prcomp(efa_data, scale. = TRUE)
 
 # Les valeurs propres sont stockees dans l'objet 'sdev' (ecart-type),
 # il faut les mettre au carre pour obtenir la variance (valeur propre).
 valeurs_propres <- acp_simple$sdev^2
 
 # On cree un tableau de donnees
 
 scree_data <- data.frame(
   Composante = 1:length(valeurs_propres),
   Valeur_Propre = valeurs_propres
 )
 
 # Maintenant, on trace le graphique
 
 ggplot(scree_data, aes(x = Composante, y = Valeur_Propre)) +
   geom_line(color = "blue", size = 1) +      
   geom_point(color = "blue", size = 3) +     
   # On ajoute une ligne pour la fameuse "Regle de Kaiser" (valeur propre > 1)
   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
   
   labs(
     title = "Éboulis des Valeurs Propres (Critère du Coude)",
     subtitle = "Pour déterminer le nombre de composantes à retenir",
     x = "Numéro de la Composante Principale",
     y = "Valeur Propre (Variance expliquée)"
   ) +
   # On s'assure que l'axe des x a des nombres entiers (1, 2, 3...)
   scale_x_continuous(breaks = 1:length(valeurs_propres)) +
   # On choisit un theme propre
   theme_minimal()
 
 # METHODE 2 : L'analyse parallele
 
 # -> On compare la courbe bleue (nos vraies donnees) a la courbe rouge (du hasard).
 # -> On garde les composantes ou le bleu est au-dessus du rouge.
 
 parallel_analysis_acp <- fa.parallel(efa_data, fa = "pc")
 
 print(parallel_analysis_acp)
 
 # --- 4.3. VISUALISATION DE L'ACP SANS ROTATION ---
 library(factoextra)
 acp_simple <- prcomp(efa_data, scale. = TRUE)
 
 # On cree le cercle de correlation pour les deux premieres composantes
 cercle_avant_rotation <- fviz_pca_var(acp_simple, 
                                       axes = c(1, 2), # On regarde le plan 1-2
                                       repel = TRUE,   
                                       title = "Cercle de Corrélation AVANT Rotation")
 
 # On affiche le graphique
 print(cercle_avant_rotation)
 
 # --- 4.4. BIPLOT AVANT ROTATION (AVEC ECHANTILLON D'INDIVIDUS) ---
 # Avec plus de 100 000 individus, on ne peut pas tous les afficher.
 # en utilise La fonction fviz_pca_biplot  elle ne trace pas tout
 # mais selectionne les individus les plus representatifs ou fait un echantillon.
 
 biplot_avant_rotation <- fviz_pca_biplot(acp_simple, 
                                          axes = c(1, 2), # On regarde le plan 1-2
                                          repel = TRUE,   # Evite que les textes des variables se chevauchent
                                          
                                          # --- Options pour les individus (points) ---
                                          geom.ind = "point",    
                                          pointshape = 21,        
                                          pointsize = 1.5,        
                                          col.ind = "gray50",     
                                          alpha.ind = 0.3,        # Transparence pour eviter une grosse tache noire
                                          
                                          # --- Options pour les variables (fleches) ---
                                          col.var = "red2",       # Couleur des fleches et du texte
                                          
                                          # --- Titre ---
                                          title = "Biplot de l'ACP - AVANT Rotation")
 
 print(biplot_avant_rotation)
 
 # --- 5. EXECUTION DE L'ACP AVEC ROTATION ---
 
 nombre_de_composantes <- 3 
 
 acp_rotee_result <- principal(efa_data, 
                               nfactors = nombre_de_composantes, 
                               rotate = "varimax",
                               scores = TRUE)
 
 print(acp_rotee_result$loadings, cutoff = 0.4, sort = TRUE)
 
 # --- 6. CERCLE DE CORRELATION APRES ROTATION ---
 # On recupere les loadings (coordonnees des variables) apres rotation
 loadings_rotees_df <- as.data.frame(unclass(acp_rotee_result$loadings))
 
 # On cree le graphique avec ggplot2 pour un rendu professionnel
 library(ggplot2)
 library(ggrepel)
 
 cercle_apres_rotation <- ggplot(loadings_rotees_df, aes(x = RC1, y = RC2, label = rownames(loadings_rotees_df))) +
   # Dessiner un cercle en arriere-plan
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
   # Ajouter les fleches pour les variables
   geom_segment(aes(x = 0, y = 0, xend = RC1, yend = RC2), 
                arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
   # Ajouter les etiquettes
   geom_text_repel(color = "steelblue", max.overlaps = 20) +
   # Assurer un repere orthonorme
   coord_fixed(ratio = 1) +
   # Ajouter les titres et noms des axes
   labs(
     title = "Cercle de Corrélation APRES Rotation Varimax",
     subtitle = "Les variables sont maintenant alignées sur les axes interprétables",
     x = "RC1 : Expérience en Cabine",
     y = "RC2 : Logistique au Sol"
   ) +
   theme_minimal()
 
 # Afficher le graphique
 print(cercle_apres_rotation)
 
 # --- 7. BIPLOT APRES ROTATION (VARIABLES + INDIVIDUS) ---
 # On recupere les resultats de notre ACP rotationnee ('acp_rotee_result')
 
 # 1. les scores des individus sur les axes rotationnes
 scores_rotees_df <- as.data.frame(acp_rotee_result$scores)
 
 # coordonnees des variables
 loadings_rotees_df <- as.data.frame(unclass(acp_rotee_result$loadings))
# on calcule un facteur d'echelle
 #    Les scores des individus et les loadings des variables n'ont pas la meme echelle.
 #    Cette ligne calcule un ratio pour que les fleches soient bien visibles
 #    par rapport au nuage de points, sans etre trop grandes ni trop petites.
 
 echelle_biplot <- max(abs(scores_rotees_df[,1:2])) / max(abs(loadings_rotees_df[,1:2])) * 0.8
 
 library(ggplot2)
 library(ggrepel)
 
 
 
 biplot_apres_rotation <- ggplot() +
   # Couche 1 : Le nuage de points des passagers (tres transparents)
   geom_point(data = scores_rotees_df, aes(x = RC1, y = RC2), color = "blue", alpha = 0.05) +
   
   # Couche 2 : Les fleches pour les variables (redimensionnees avec notre echelle)
   geom_segment(data = loadings_rotees_df, 
                aes(x = 0, y = 0, xend = RC1 * echelle_biplot, yend = RC2 * echelle_biplot),
                arrow = arrow(length = unit(0.2, "cm")), color = "red2") +
   
   # Couche 3 : Les etiquettes des variables (a l'extremite des fleches)
   geom_text_repel(data = loadings_rotees_df, 
                   aes(x = RC1 * echelle_biplot, y = RC2 * echelle_biplot, label = rownames(loadings_rotees_df)),
                   color = "red2", fontface = "bold", max.overlaps = 15) +
   
   # Couche 4 : Les axes et les titres
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
   coord_fixed(ratio = 1) + # Tres important pour que les angles soient corrects
   labs(
     title = "Biplot APRES Rotation Varimax",
     subtitle = "Projection des passagers et des variables sur les axes interprétables",
     x = "RC1 : Expérience en Cabine",
     y = "RC2 : Logistique et Commodité"
   ) +
   theme_minimal()
 
 #le graphique avec rotation
 
 print(biplot_apres_rotation)
 
 # --- 7. (VERSION AMELIOREE ET CORRIGEE) BIPLOT PROFESSIONNEL APRES ROTATION ---
 
 # --- Preparation des donnees ---
 # On recupere les scores des individus sur les axes rotationnes
 scores_rotees_df <- as.data.frame(acp_rotee_result$scores) # <--- CORRIGE ICI
 
 # On recupere les loadings (coordonnees) des variables
 loadings_rotees_df <- as.data.frame(unclass(acp_rotee_result$loadings)) # <--- CORRIGE ICI
 
 # On doit recuperer la colonne "satisfaction" de nos donnees initiales.
 # C'est un peu technique : on doit s'assurer de prendre les bonnes lignes,
 # celles qui n'ont pas ete supprimees lors du nettoyage.
 indices_conserves <- as.numeric(rownames(efa_data))
 scores_rotees_df$satisfaction <- data_clean$satisfaction[indices_conserves]
 
 # --- Calcul des positions moyennes (centroides) des groupes ---
 centroides <- scores_rotees_df %>%
   group_by(satisfaction) %>%
   summarise(
     RC1_moyen = mean(RC1),
     RC2_moyen = mean(RC2)
   )
 
 # --- Creation du Biplot avec ggplot2 ---
 library(ggplot2)
 library(ggrepel)
 
 biplot_final <- ggplot() +
   # Couche 1 : Les fleches pour les variables (on les rend un peu plus courtes pour la lisibilite)
   geom_segment(data = loadings_rotees_df, 
                aes(x = 0, y = 0, xend = RC1 * 2, yend = RC2 * 2), # Facteur d'echelle *2
                arrow = arrow(length = unit(0.2, "cm")), color = "gray50") +
   
   # Couche 2 : Les etiquettes des variables
   geom_text_repel(data = loadings_rotees_df, 
                   aes(x = RC1 * 2.2, y = RC2 * 2.2, label = rownames(loadings_rotees_df)), # On eloigne un peu le texte
                   color = "gray30", fontface = "bold") +
   
   # Couche 3 : Les points pour les centroides des groupes (Satisfait vs Insatisfait)
   geom_point(data = centroides, 
              aes(x = RC1_moyen, y = RC2_moyen, color = satisfaction), 
              size = 6, alpha = 0.8) +
   
   # Couche 4 : Les etiquettes des groupes
   geom_text_repel(data = centroides, 
                   aes(x = RC1_moyen, y = RC2_moyen, label = satisfaction),
                   box.padding = 1.5, fontface = "bold", size = 5) +
   
   # Couche 5 : Les axes et les titres
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
   coord_fixed(ratio = 1) +
   labs(
     title = "Analyse Biplot de la Satisfaction Client",
     subtitle = "Positionnement des profils de passagers sur les axes de satisfaction",
     x = "RC1 : Expérience en Cabine",
     y = "RC2 : Logistique et Commodité"
   ) +
   theme_minimal(base_size = 14) + # Augmente la taille de la police
   theme(legend.position = "none") # On enleve la legende devenue inutile
 
 # On affiche le graphique final
 print(biplot_final)