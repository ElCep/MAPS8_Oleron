/**
 *  SPRITE
 *  Author: Mog
 *  Description: 
 */
model SPRITE

/* Insert your model definition here */
global
{
	//chargement des données géographiques
	file island_shapefile <- file("../includes/contours_ile.shp");
	file sea_shapefile <- file("../includes/mer3.shp");
	file dykes_shapefile <- file("../includes/ouvrages.shp");
	file buildings_shapefile <- file("../includes/batiments.shp");
	file dem_file <- csv_file("../includes/mnt_small.csv", " ");

	//définition des géométries
	geometry shape <- envelope(file("../includes/rect.shp"));
	geometry lamer <- geometry(first(island_shapefile));

	//définition des cellules de mer
	list<parcelle> sea_cells;

	//un bool utiliser pour lancer le reflexe de flood TRUE = flowing
	bool phase_sub <- false;

	//********************* PARTIE MAIRIE ******************************
	float taux_impots <- 0.1;
	int budget update: sum(parcelle collect each.impots);

	//********************* PARTIE SUBMERSION DU GLOBAL*****************
	//définition des paramètres du modèle
	// taux de diffusion de l'eau d'une case à une autre - paramètre à recaller
	float diffusion_rate <- 0.6;
	float hauteur_eau <- 2.0 # m;
	float temps_submersion <- 5 # h;

	//définition pas de temps
	float step <- 1 °h;

	//paramètre des digues 
	float dyke_height <- 15.0;
	float dyke_width <- 15.0;

	//initialisation des variables
	init 
	{
	// l'utilisateur (agent controle par le joueur via des boutons)
	//create user;
		do init_cells;
		do init_water;
		do init_obstacles;
		create territoire number:1 ;
		sea_cells <- parcelle where (each.is_sea);
		ask parcelle
		{
			do update_color;
		}

		do placer_digues_maisons;
		//do color_bati;
	}
	// fin init

	//initialisation des cellules de la grille a partir du shapefile
	action init_cells
	{
		matrix data <- matrix(dem_file);
		ask parcelle
		{
			altitude <- float(data[grid_x, grid_y]);
			altitude <- max([-2, altitude]);
			int alt_color <- max([0, min([255, int(255 * (1 - (3 + altitude) / 25))])]);
			color <- rgb([alt_color, 255, alt_color]);
			neighbour_cells <- (self neighbours_at 1);
		}

	}

	//initialisation de la mer a partir du shapefile
	action init_water
	{
		geometry sea <- geometry(sea_shapefile);
		ask parcelle overlapping sea
		{
			water_height <- hauteur_eau;
			is_sea <- true;
		}
	}

	//initialisation des bâtiments et des digues a partir du shapefile
	action init_obstacles
	{
	//création des bâtiments à partir des fichiers géo
		create building from: buildings_shapefile
		{
			loop i from: 0 to: length(shape.points) - 1
			{
				shape <- set_z(shape, i, 0.0);
			}

			do update_cells;
		}

		//création des bâtiments à partir des fichiers géo (avec récup de la hauteur et de l'état)
		create dyke from: dykes_shapefile with: [hauteur::float(read("hauteur")), etat::string(read("Etat_Ouvra"))];
		ask dyke
		{
		// passage de mm (dans fichier) à des m
			height <- hauteur / 100;
			shape <- shape + dyke_width;
			do update_cells;
		}

	}

	//renseigne sur la présence de digue sur une cellule
	action placer_digues_maisons
	{
		ask parcelle overlapping dyke
		{
			digue <- true;
		}
		ask parcelle overlapping building {
			maison <- true;
			bats <- building overlapping self;
			//surface d'une cellule 200x200 et surface_maison, taux de surface couverte par le bati en %
			densite_bati <- sum(bats collect each.shape.area) / 400;
		}
	}

/* 	action color_bati {
		ask parcelle {
			if not empty(building overlapping self) {
				maison <- true;
				bats <- building overlapping self;
				//surface d'une cellule 200x200 et surface_maison, taux de surface couverte par le bati en %
				surface_maison <- sum(bats collect each.shape.area) / 400;
			}
		}
	}
*/
	//régénartion de l'eau dans les cellules de mer (pour simuler le remplacement de l'eau)
	reflex adding_input_water when: phase_sub
	{
		float water_input <- rnd(100) / 100;
		ask sea_cells
		{
			water_height <- water_height + water_input;
		}

	}

	//mécanisme de submersion  -demande de propagation aux cellules
	reflex flowing when: phase_sub
	{
		ask parcelle
		{
			already <- false;
		}

		ask (parcelle sort_by ((each.altitude + each.water_height + each.obstacle_height)))
		{
			do flow;
		}

	}

	//mise à jour de la couleur des cellules en fonction de l'eau
	reflex update_cell_color when: phase_sub
	{
		ask parcelle
		{
			do update_color;
		}

		do placer_digues_maisons;
		//do color_bati;
	}
	// reflexe de la parcelle de mettre a jour sa couleur
	// les digues et maisons ne bougent pas: pas besoin de mise a jour

	//condition de fin - a partir de t>tfin, la diffusion diminue
	reflex fin_submersion when: phase_sub
	{
		if time > temps_submersion
		{
			diffusion_rate <- max([0, diffusion_rate - 0.1]);
		}
	}
	
	// TODO condition de transition entre phase submersion et phaase interactive
	reflex transition_submersion {
		// si phase_sub depuis tant de temps / inondation finie
		// alors passer en phase interactive
		
		// si phase interactive
		// et tout le budget depense / clic sur "terminer tour" / temps ecoule / clic bouton "lancer la flotte"
		// alors passer en phase sub
	}

	// il faudra avancer la simulation step by step a la main	
	user_command transition {
		if phase_sub {phase_sub<-false; write 'passage en phase interactive';}
		else {phase_sub<-true; write 'passage en phase submersion';}
	}



}
/* ******************************************************************
 ******* fin global *******                                       ***
*********************************************************************/

species territoire {
	
	// surface habitable = nombre de cellules terrestres * densite bati
	// pourcentage bati de l'ile
	float surface_habitable {
		sum ((parcelle where !each.is_sea) collect each.densite_bati)
	}

	//  20000 habitants en tout / surface habitable = population max par cellule
	// nb d'habitants par %age de surface batie 
	float densite_population {
		surface_habitable/20000
	}
	
	// population totale
	

	/**********************************
	 * *** CALCUL DES INDICATEURS *** *
	 **********************************/
	 
	 // popularite en fonction de la satisfaction ponderee de chaque cellule (satisfaction*densite population)
	 int indicateurPopularite {
	 	sum( parcelle collect each.satisfactionPonderee) / sum(parcelle collect each.densite_bati)
	 }


	
}


/********************************
* OBSTACLES : maisons et digues *
*********************************/

//spécification des obstacles et maison
species obstacle
{
	float height min: 0.0;
	string etat;
	rgb color;
	float water_pressure update: compute_water_pressure();
	list<parcelle> cells_concerned;
	list<parcelle> cells_neighbours;
	float compute_water_pressure
	{
		if (height = 0.0)
		{
			return 0.0;
		} else
		{
			if (not empty(cells_neighbours))
			{
				float water_level <- cells_neighbours max_of (each.water_height);
				return min([1.0, water_level / height]);
			}

		}

	}

	action update_cells
	{
		cells_concerned <- (parcelle overlapping self);
		ask cells_concerned
		{
			add myself to: obstacles;
			water_height <- 0.0;
		}

		cells_neighbours <- cells_concerned + cells_concerned accumulate (each.neighbour_cells);
		do compute_height();
		if (height > 0.0)
		{
			water_pressure <- compute_water_pressure();
		} else
		{
			water_pressure <- 0.0;
		}

	}

	// ???
	action compute_height;
	
	aspect geometry
	{
		int val <- int(255 * water_pressure);
		color <- rgb(val, 255 - val, 0);
		draw shape color: color depth: height border: color;
	}

}

species building parent: obstacle
{
	float height <- 2.0 + rnd(8);
}

species dyke parent: obstacle
{
	int counter_wp <- 0;
	int hauteur;
	int breaking_threshold <- 24;
	action break
	{
		ask cells_concerned
		{
			do update_after_destruction(myself);
		}

		do die;
	}

	// FIXME: est-ce qu'il faut copier la meme action dans building?
	// dans ce cas il faut plutot la mettre dans l'espece parente = obstacle
	action compute_height
	{
		height <- dyke_height - mean(cells_concerned collect (each.altitude));
	}

	reflex breaking_dynamic when: phase_sub
	{
		if (water_pressure = 1.0)
		{
			counter_wp <- counter_wp + 1;
			if (counter_wp > breaking_threshold)
			{
				do break;
			}

		} else
		{
			counter_wp <- 0;
		}

	}

}




/***************************************
 * ******* GRILLE DE PARCELLES ******* *
 ***************************************/

grid parcelle width: 52 height: 90 neighbours: 8 frequency: 0 use_regular_agents: false use_individual_shapes: false use_neighbours_cache: false
{
	/***************************Variables pour flood****************************************/

	// altitude d'apres le MNT
	float altitude;

	// hauteur d'eau sur la cellule
	float water_height <- 0.0 min: 0.0;

	// hauteur du plus haut obstacle si plusieurs
	float obstacle_height <- 0.0;

	// hauteur totale agreegee = altitude + hauteur bati + hauteur eau
	float height;

	// cellules voisines (Moore, 8)
	list<parcelle> neighbour_cells;

	// cellule mer / terre 
	bool is_sea <- false;

	// liste des obstacles situes sur cette cellule      
	list<obstacle> obstacles;

	// est-ce que la cellule a deja ete traitee dans la diffusion de l'eau
	bool already <- false;

	// couleur initiale (sera mise a jour) 
	rgb color <- #green;

	// calculer la hauteur du plus haut obstacle present sur cette cellule
	float compute_highest_obstacle
	{
	// si aucun obstacle : hauteur nulle
		if (empty(obstacles))
		{
			return 0.0;
		}
		// sinon renvoyer le max
		else
		{
			return obstacles max_of (each.height);
		}

	}

	//action de transmission de la submersion
	action flow
	{
		// s'il y a de l'eau sur la cellule, il faut la diffuser
		if (water_height > 0)
		{
			// trouver la liste des voisins deja traites pour la diffusion
			list<parcelle> neighbour_cells_al <- neighbour_cells where (each.already);
			// si cette liste n'est pas vide
			if (!empty(neighbour_cells_al))
			{
				// la hauteur de ces voisins devient egale a alt+water+obstacle
				ask neighbour_cells_al
				{
					height <- altitude + water_height + obstacle_height;
				}
				// la hauteur de la cellule vaut altitude + hauteur d'eau
				height <- altitude + water_height;
				// cellules cibles de la diffusion = celles de hauteur plus basse que la cellule courante
				list<parcelle> flow_cells <- (neighbour_cells_al where (height > each.height));
				// s'il y a des cellules plus basses
				if (!empty(flow_cells))
				{
					loop flow_cell over: shuffle(flow_cells) sort_by (each.height)
					{
						float water_flowing <- max([0.0, min([(height - flow_cell.height), water_height * diffusion_rate])]);
						water_height <- water_height - water_flowing;
						flow_cell.water_height <- flow_cell.water_height + water_flowing;
						height <- altitude + water_height;
					}
				}
			}
		}
		already <- true;
	}

	// mise a jour couleur en fonction de la hauteur d'eau
	// TODO: a remplacer par un aspect specifique pour visualisation de l'eau
	action update_color
	{
		int val_water <- 0;
		val_water <- max([0, min([255, int(255 * (1 - (water_height / 1.0)))])]);
		color <- rgb([val_water, val_water, 255]);
		//grid_value <- water_height + altitude;
	}

	action update_after_destruction (obstacle the_obstacle)
	{
		// retirer l'obstacle de la liste d'obstacles presents sur cette cellule
		remove the_obstacle from: obstacles;
		// et mettre a jour la hauteur totale d'obstacles (qui baisse en consequence)
		obstacle_height <- compute_highest_obstacle();
	}

	/****************************Variables interactions parcelles **********************/

	// il y a une digue sur la parcelle	
	bool digue <- false;
	//bool ttest <- !empty(dyke in agents_overlapping(self));
	//bool has_dyke update: self.geometry overlaps dyke;

	// il est possible de construire sur cette parcelle (pas en zone noire)
	bool constructible <- true;

	// il y a une maison sur cette parcelle
	list<building> bats;
	float densite_bati <- 0.0;
	bool maison <- false;

	// valeur ecologique
	int valeurEcolo <- rnd(10) min: 0 max: 10;

	// valeur attractivite
	int valeurAttractivite <- rnd(10) min: 0 max: 10;

	//valeur securite
	int valeurSecurite <- rnd(10) min: 0 max: 10;

	// impots donnes par cette parcelle a la mairie en fonction de sa population+attractivite
	int impots update:		taux_impots*(valeurAttractivite); // + agents_overlapping(self)
	

	// satisfaction ecologique inversement proportionnelle a la distance vers la plus proche cellule de haute valeur ecologique
	// attention closest_to n'est pas optimise
	//parcelle closestBeach <- ((parcelle where (each.valeurEcolo>7)) closest_to(self));  
	//cell closestBeach <- ((cell where (each.valeurEcolo>7)) closest_to(self));  
	//float distanceBeach <- self distance_to closestBeach;

	/*int satisfNormalisee  {
		if (distanceBeach <= 2) {return 1;}
		else if (distanceBeach <=5) { return 0.5;}
		else {return 0;}
	}*/

	// la satisfaction des habitants de cette parcelle est la somme des 3 indices (secu, ecolo, attractivite)
	int valeurSatisfaction function: { valeurSecurite + valeurAttractivite + valeurEcolo };

	// satisfaction ponderee par la population - valeur entre 0 et 1000 (densite bati entre 1 et 100)
	int satisfactionPonderee {
		valeurSatisfaction*densite_bati
	}

	/*********************************
	 * *** REFLEXES DE LA PARCELLE ***
	 *********************************/

	// 3 valeurs
	// - securite
	// secu augmente avec digues (++ si vraie digue, + si digue ecolo) et avec densite population et actions information
	// secu diminue avec proximite a la mer, et avec frequence/recence/gravite (ie hauteur d'eau) de la derniere inondation
	// - ecologie
	// ecolo augmente avec actions conservation et avec expropriation (direct par non constructibilite, indirect par diminution densite population)
	// ecolo diminue (--) avec digue standard, diminue un peu (-) avec digue ecolo, diminue avec densite population
	// - attractivite
	// attract augmente avec action promotion, avec proximite mer
	// attract diminue avec digues standard et avec densite population
	
	// valeur d'impots de la parcelle depend de population et de attractivite (retombees touristiques)

	// reflexe pour la mise a jour de la valeur ecologique a chaque tour
	reflex updateValeurEcolo when: phase_sub
	{
	// si pas de digue : augmente

	// si digue ou toute construction : diminue

	// non constructibilite : augmente
	}

	// reflexe pour la mise a jour de la valeur touristique a chaque tour
	reflex updateValeurAttractiv when: phase_sub
	{
	// proximite a la mer, constructibilite
	}

	// reflexe 
	reflex updateValeurSecurite when: phase_sub
	{
	// depend des digue, de l'information,densite
	}

	/************************************
	 * *** ACTIONS DE L'UTILISATEUR *** *
	 * *** JEU INTERACTIF           *** *
	 ************************************/
	user_command "construire digue" action: construire_digue;
	action construire_digue
	{
		digue <- true;
	}

	user_command "interdire construction" action: interdire_construction;
	action interdire_construction
	{
		constructible <- false;
	}

	action construire_maison
	{
		maison <- true;
	}

	// affichage graphique des parcelles
	aspect default
	{
	// zone noire = rond noir
		if (!constructible)
		{
			draw circle(210 # m) color: # black;
		}

		// maison = carre bleu
		if (maison)
		{
			draw square(150 # m) color: # blue;
		}

		// digue = triangle jaune
		if (digue)
		{
			draw triangle(100 # m) color: # yellow;
		}

	}

	aspect ecolo {
		rgb ze_colour <- #white;
		if (is_sea) {ze_colour <- #blue;}
		// degrade de vert pour valeur ecolo
		else {ze_colour <- rgb(255-25.5*valeurEcolo,255,255-25.5*valeurEcolo);}

		draw square(self.shape.perimeter) color: ze_colour;
		//if (is_sea) {color <- #blue;}
	}
	
	aspect water {
		int val_water <- max([0, min([255, int(255 * (1 - (water_height / 1.0)))])]);
		color <- rgb([val_water, val_water, 255]);
	}

}

/********************
 * *** SIMULATION ***
 ********************/
experiment Displays type: gui
{
//Definition de quelques parametres
	parameter "Phase de submersion ?: " var: phase_sub;
	output
	{
		display map ambient_light: 100
		{
			grid parcelle triangulation: false lines: # black;
			species parcelle aspect: default;
			graphics "toto"
			{
				draw lamer.contour color: # yellow;
			}
			//  species dyke aspect: geometry;
			//image "toto" gis: "../includes/contours_ile.shp" color: #red;
			//	species building aspect: geometry refresh: false;
		}

		// carte des valeurs ecolo
		display map_ecolo ambient_light: 100
		{
			grid parcelle triangulation: false lines: # black;
			species parcelle aspect: ecolo;
		}

		// carte des valeurs d'attractivite en jaune
		
		// carte de densite de population avec diametre du cercle dans la cellule
		
		// carte de securite en rouge

		/* rajouter des graphiques de visualisation des differents indicateurs */
		
		display ChartHisto {
			chart "DataBar" type:histogram
			{
				data "nombre de satisfaits" value:(list(parcelle) count (each.valeurSatisfaction > 5)) color:°red;
				data "popularite" value:any(territoire).indicateurPopularite;
//				data "carry_food_ants" value:(list(ant) count (each.hasFood)) color:°green;				
			}
			
			}
		
		
	}

}
