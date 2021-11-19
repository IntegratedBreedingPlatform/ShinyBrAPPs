/*global angular*/
'use strict';

(function() {

	var bmsAuth = angular.module('bmsAuth', ['LocalStorageModule']);

	bmsAuth.factory('authInterceptor', ['localStorageService', function(localStorageService) {
		return {
			// Add authorization token to headers
			request: function(config) {
				config.headers = config.headers || {};
				var token = localStorageService.get('xAuthToken');

				if (token && token.expires && !config.overrideAuthToken) {
					config.headers['x-auth-token'] = token.token;
				}

				return config;
			}
		};
	}]);

	bmsAuth.factory('authExpiredInterceptor', ['$q', 'localStorageService', 'reAuthenticationService', function($q, localStorageService,
																														  reAuthenticationService) {
		return {
			responseError: function(response) {
				// Token has expired or is invalid.
				if (response.status === 401) {
					localStorageService.remove('xAuthToken');
					reAuthenticationService.handleReAuthentication();
				}
				return $q.reject(response);
			}
		};
	}]);

	bmsAuth.service('reAuthenticationService', function() {
		var hasBeenHandled = false;
		return {
			// Current strategy to re-authenticate is to log the user out from Workbench by hitting Spring security internal logout endpoint
			//    which means re-login, which in turn means a fresh token will be issued ;)
			// TODO find a better alternative to use insead of alert then in the face punch to logout which is easy to unit test as well.
			handleReAuthentication: function() {
				if (!hasBeenHandled) {
					hasBeenHandled = true;
					alert('Breeding Management System needs to authenticate you again. Redirecting to login page.');
					window.top.location.href = '/ibpworkbench/logout';
				}
			}
		};
	});

})();
