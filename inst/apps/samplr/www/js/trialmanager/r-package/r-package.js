(function () {
	'use strict';

	var rPackageModule = angular.module('r-package', ['fieldbook-utils']);

	rPackageModule.factory('rPackageService', ['$http', 'serviceUtilities', function ($http, serviceUtilities) {

		var successHandler = serviceUtilities.restSuccessHandler,
			failureHandler = serviceUtilities.restFailureHandler;

		var rPackageService = {};

		rPackageService.getRCallsObjects = function (packageId) {
			return $http({
				method: 'GET',
				url: '/bmsapi/r-packages/' + +packageId + '/r-calls'
			}).then(successHandler, failureHandler);
		};

		rPackageService.executeRCall = function (url, parameters) {
			return $http({
				method: 'POST',
				url: url,
				data: $.param(parameters),
				headers: {'Content-Type': 'application/x-www-form-urlencoded'},
				overrideAuthToken: true
			}).error(rCallErrorHandler);
		};

		function rCallErrorHandler(data, status) {
			if (status === 400) {
				// http status 400 (bad request) means OpenCPU cannot process the request due to bad/invalid data.
				showErrorMessage('', $.fieldbookMessages.errorPlotGraphGeneration);
			} else {
				showErrorMessage('', $.fieldbookMessages.errorPlotGraphGenericError);
			}
		}

		return rPackageService;
	}]);


})();