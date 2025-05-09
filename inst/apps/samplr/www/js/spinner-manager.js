/* global Spinner */
window.SpinnerManager = (function() {
	'use strict';
	var activeConnectionsAvailable = false;
	var DELAY = 1000;

	function startSpinnerIfNecessary() {
		if (activeConnectionsAvailable) {
			Spinner.play();
		}
	}

	var service = {
		addActive: function() {
			service.addActiveWithCustomDelay(DELAY);
		},

		addActiveWithCustomDelay: function(timeoutDelay) {
			if (!activeConnectionsAvailable) {
				setTimeout(startSpinnerIfNecessary, timeoutDelay);
				activeConnectionsAvailable = true;
			}
		},

		resolveActive: function() {
			activeConnectionsAvailable = false;

			Spinner.stop();
		}
	};

	return service;

}());
