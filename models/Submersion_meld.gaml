/**
 *  Submersion
 *  Author: SPRITE team, repris du modèle Flood simulation de Patrick Taillandier
 *     (modele hydro du projet Archives)
 *  Description: modèle de sumbermision de l'Ile d'Oléron
 */

model Submersion


global {
 //chargement des données géographiques
  file island_shapefile <- file("../includes/contours_ile.shp");
  file sea_shapefile <- file("../includes/mer3.shp");
  file dykes_shapefile <- file("../includes/ouvrages.shp");
  file buildings_shapefile <- file("../includes/bati.shp");
  file dem_file <- csv_file("../includes/mnt_small.csv"," ");  
  
  //définition des paramètres du modèle
  // taux de diffusion de l'eau d'une case à une autre - paramètre à recaller
  float diffusion_rate <- 0.6;
  float hauteur_eau <- 2.0 #m;
  float temps_submersion <- 5 #h;
  		
  //définition pas de temps
  float step <- 1°h;

	//paramètre des digues 
  	float dyke_height <- 15.0;
    float dyke_width <- 15.0;
     
  	//définition des géométries
   	geometry shape <- envelope(file("../includes/rect.shp"));
   	geometry lamer <- geometry(first(island_shapefile));
 
	//définition des cellules de mer
   	list<cell> sea_cells;
 
   	//initialisation des variables
  	init {
    	do init_cells;
     	do init_water;
     	do init_obstacles;
    	sea_cells <- cell where (each.is_sea);
        ask cell {
         	do update_color;
      	}
      	do color_dyke;
   	}// fin init
   
   	//initialisation des cellules de la grille
   	action init_cells {
    	matrix data <- matrix(dem_file);
		ask cell {
      		altitude <- float(data[grid_x,grid_y]);
        	altitude <- max([-2,altitude]);
        	int alt_color <- max([0, min([255, int(255 * (1 - (3+altitude)/25))])]) ;  
        	color <- rgb([alt_color, 255, alt_color]);
        	neighbour_cells <- (self neighbours_at 1) ;
   		}
   	}
   
   		//initialisation de la mer
   	action init_water {
    	geometry sea <- geometry(sea_shapefile);
  		ask cell overlapping sea {
    		water_height <- hauteur_eau;
      		is_sea <- true;
    	}
   	}
   
   	//initialisation des bâtiments et des digues
 	action init_obstacles{
    	//création des bâtiments à partir des fichiers géo
    	create building from: buildings_shapefile {
      		loop i from: 0 to: length(shape.points) - 1 {
      			set shape <-  set_z (shape, i, 0.0);
      		} 
         	do update_cells;
     	 }
    
    	//création des bâtiments à partir des fichiers géo (avec récup de la hauteur et de l'état)
      	create dyke from: dykes_shapefile with:[hauteur::float(read ("hauteur")),etat::string(read("Etat_Ouvra"))];
  		ask dyke {
			// passage de mm (dans fichier) à des m
        	height <-hauteur/100;
         	shape <-  shape + dyke_width;
            do update_cells;
      	}
      }
      
    action color_dyke {
      	ask cell overlapping dyke {
   			//write ("test");
   		 	color <- #red; 
   		}
   	}
   
   //régénartion de l'eau dans les cellules de mer (pour simuler le remplacement de l'eau)
   reflex adding_input_water {
   	  float water_input <- rnd(100)/100;
      ask sea_cells {
         water_height <- water_height + water_input;
      }
   }
   
   	//mécanisme de submersion  -demande de propagation aux cellules
   	reflex flowing {
      ask cell {already <- false;}
  	  ask (cell sort_by ((each.altitude + each.water_height + each.obstacle_height))) {
       do flow;
      }
   }
   
   //mise à jour de la couleur des cellules en fonction de l'eau
   reflex update_cell_color {
      ask cell {
         do update_color;
      }
   }
  
	//condition de fin - a partir de t>tfin, la diffusion diminue
	reflex fin_submersion {
	if time>temps_submersion {
		diffusion_rate<-max([0,diffusion_rate-0.1]);
	}
}
}


//spécification des obstacles et maison
    species obstacle {
      float height min: 0.0;
      string etat;
      rgb color;
      float water_pressure update: compute_water_pressure();
      
      list<cell> cells_concerned ;
      list<cell> cells_neighbours;
      float compute_water_pressure {
         if (height = 0.0) {
            return 0.0;
         } else {
         			if (empty(cells_neighbours)) {
         			do die;
         	}
            float water_level <- cells_neighbours max_of (each.water_height);
            return min([1.0,water_level / height]);
         } 
      }
      action update_cells {
         cells_concerned <- (cell overlapping self);
        	ask cells_concerned {
            add myself to: obstacles;
            water_height <- 0.0;
         }
         cells_neighbours <- cells_concerned + cells_concerned accumulate (each.neighbour_cells);
      	 do compute_height();
         if (height > 0.0) {   
            water_pressure <- compute_water_pressure();
         } else {water_pressure <- 0.0;}
      }
      action compute_height;
      aspect geometry {
         int val <- int( 255 * water_pressure);
         color <- rgb(val,255-val,0);
         draw shape color: color depth: height border: color;
      }
   }
  species building parent: obstacle {
      float height <- 2.0 + rnd(8);
   }
   
   species dyke parent: obstacle{
       int counter_wp <- 0;
       int hauteur;
       int breaking_threshold <- 24;
      
       action break{
         ask cells_concerned {
            do update_after_destruction(myself);
         }
         do die;
      }
      
      action compute_height
       {
      	   height <- dyke_height - mean(cells_concerned collect (each.altitude));
      }
      
      reflex breaking_dynamic {
      	if (water_pressure = 1.0) {
      		counter_wp <- counter_wp + 1;
      		if (counter_wp > breaking_threshold) {
      			do break;
      		}
      	} else {
      		counter_wp <- 0;
      	}
      }
      user_command "Destroy dyke" action: break; 
   }
   
   

   
grid cell width: 52 height: 90 neighbours: 8 frequency: 0  use_regular_agents: false use_individual_shapes: false use_neighbours_cache: false {   
//grid cell width: 251 height: 374 neighbours: 8 frequency: 0  use_regular_agents: false use_individual_shapes: false use_neighbours_cache: false {
      
      // altitude d'apres le MNT
      float altitude;
      
      // hauteur d'eau sur la cellule
      float water_height <- 0.0 min: 0.0;

      // hauteur du plus haut obstacle si plusieurs
      float obstacle_height <- 0.0;
      
      // hauteur totale agreegee = altitude + hauteur bati + hauteur eau
      float height;
      
      // cellules voisines (Moore, 8)
      list<cell> neighbour_cells ;
      
      // cellule mer / terre 
      bool is_sea <- false;
      
	  // liste des obstacles situes sur cette cellule      
      list<obstacle> obstacles;
      
      // est-ce que la cellule a deja ete traitee dans la diffusion de l'eau
      bool already <- false;
      
      // couleur initiale (sera mise a jour) 
      rgb color <- #green;
       
     // calculer la hauteur du plus haut obstacle present sur cette cellule
    float compute_highest_obstacle {
    	// si aucun obstacle : hauteur nulle
         if (empty(obstacles)) {return 0.0;}
         // sinon renvoyer le max
         else { return obstacles max_of(each.height);}
      }
      
     //action de transmission de la submersion
      action flow {
      	 // s'il y a de l'eau sur la cellule, il faut la diffuser
         if (water_height > 0) {
         	// trouver la liste des voisins deja traites pour la diffusion
            list<cell> neighbour_cells_al <- neighbour_cells where (each.already);
            // si cette liste n'est pas vide
            if (!empty(neighbour_cells_al)) {
             	// la hauteur de ces voisins devient egale a alt+water+obstacle
             	ask neighbour_cells_al {height <- altitude + water_height + obstacle_height;
             }
             // la hauteur de la cellule vaut altitude + hauteur d'eau
             height <-  altitude +  water_height;
             // cellules cibles de la diffusion = celles de hauteur plus basse que la cellule courante
             list<cell> flow_cells <- (neighbour_cells_al where (height > each.height)) ;
             // s'il y a des cellules plus basses
             if (!empty(flow_cells)) {
                  loop flow_cell over: shuffle(flow_cells) sort_by (each.height){
                     float water_flowing <- max([0.0, min([(height - flow_cell.height), water_height * diffusion_rate])]); 
                     water_height <- water_height - water_flowing;
                     flow_cell.water_height <-flow_cell.water_height +  water_flowing;
                     height <- altitude + water_height;
                  }   
               }
            }
         }
         already <- true;
      }  
      action update_color { 
         int val_water <- 0;
         val_water <- max([0, min([255, int(255 * (1 - (water_height / 1.0)))])]) ;  
         color <- rgb([val_water, val_water, 255]);
         grid_value <- water_height + altitude;  
      }
     
      action update_after_destruction(obstacle the_obstacle){
         remove the_obstacle from: obstacles;
         obstacle_height <- compute_highest_obstacle();
      }
      
   }


experiment main_gui type: gui {
   parameter "Shapefile for the sea" var:sea_shapefile category:"Water data";
  // parameter "Shapefile for the dykes" var:dykes_shapefile category:"Obstacles";
  // parameter "Shapefile for the buildings" var:buildings_shapefile category:"Obstacles";
  // parameter "Height of the dykes" var:dyke_height category:"Obstacles";
   parameter "Diffusion rate" var:diffusion_rate category:"Water dynamic";
   output { 
      display map type: opengl ambient_light: 100{
         grid cell triangulation: false lines:#black;
         graphics "toto" {
        	draw lamer.contour color: #yellow;
         }
         
         //image "toto" gis: "../includes/contours_ile.shp" color: #red;
   //  species building aspect: geometry;
       species dyke aspect: geometry;
      }
     /*  display chart_display refresh_every: 24 { 
         chart "Pressure on Dykes" type: series {
            data "Mean pressure on dykes " value: mean(dyke collect (each.water_pressure)) style: line color: rgb("magenta") ;
            data "Rate of dykes with max pressure" value: (dyke count (each.water_pressure = 1.0))/ length(dyke) style: line color: rgb("red") ;
            data "Rate of dykes with high pressure" value: (dyke count (each.water_pressure > 0.5))/ length(dyke) style: line color: rgb("orange") ;
            data "Rate of dykes with low pressure" value: (dyke count (each.water_pressure < 0.25))/ length(dyke) style: line color: rgb("green") ;
         }
      }*/
   }
}