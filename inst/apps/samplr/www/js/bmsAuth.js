'use strict';

/**
 * As bmsAuth is needed from non-angular app like Nursery Manager
 * we create this as global
 *
 * TODO When migrating to angular, transform this to a service
 */
var bmsAuth = (function(bmsAuth, window) {

	// TODO see Workbench/src/main/web/src/apps/ontology/app-services/bmsAuth.js
	bmsAuth.handleReAuthentication = function() {
		alert('Breeding Management System needs to authenticate you again. Redirecting to login page.');
		window.top.location.href = '/ibpworkbench/logout';
	};

	bmsAuth.isValidToken = function() {

		var isTokenValid = true;

		// Send the authorization token with the web request.
		// If the token is not valid, the web method will return a '401 Unauthorized' response.
		$.ajax({
			url: '/bmsapi/validateToken',
			type: 'GET',
			async: false,
			beforeSend: function(xhr) {
				var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;
				xhr.setRequestHeader('X-Auth-Token', xAuthToken);
			},
			error: function(jqxhr, textStatus, error) {
				if (jqxhr.status == 401) {
					isTokenValid = false;
				}
			}
		});

		return isTokenValid;

	};

	return bmsAuth;

})(bmsAuth || {}, window);
