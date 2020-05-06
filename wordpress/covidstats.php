<?php
/**
 * @package Covid_Stats
 * @version 1.0
 */
/*
Plugin Name: Covid-19 Statistics Displayer
Plugin URI: https://moduloinfo.ca/wordpress/
Description: This plugin allow you to display the latest covid statistics and previsions for the entire world using [covidstats] shortcode on the page where you want the stats to display.
Author: Carl Sansfacon
Version: 1.0
Author URI: https://moduloinfo.ca/
*/




function carlsansshowcovidstats() {
	if(!is_admin()){
		include( plugin_dir_path( __FILE__ ) . 'covidsearch.php');
	}
}
add_shortcode('covidstats', 'carlsansshowcovidstats');
