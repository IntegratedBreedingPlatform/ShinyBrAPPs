/*
Loading throbber.
*/
window.Spinner = (function() {
	'use strict';
	function startThrobber() {
		$.blockUI({ message: '<div id="spinner-wrap"><img src="/Fieldbook/static/img/loading-animation.gif"/></div>' });
		$('.blockPage').css('border', '0px');
	}

	return {

		play: function() {
			startThrobber();
		},

		// This method is intended to be called when your asynchronous method has returned (e.g. you have hit the server and come back)
		stop: function() {
			// Stop the throbber if it has already started playing.
			$.unblockUI();
		}
	};

}());
