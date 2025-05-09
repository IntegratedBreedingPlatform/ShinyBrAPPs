(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.factory('lotService', ['$http', 'serviceUtilities', 'studyContext',
		function ($http, serviceUtilities, studyContext) {
			var successHandler = serviceUtilities.restSuccessHandler,
				failureHandler = serviceUtilities.restFailureHandler;

			var BASE_URL = '/bmsapi/crops/' + studyContext.cropName + '/lots';

			var service = {};

			service.saveSearchRequest = function (searchRequest) {
				const programUUID = studyContext.programId;
				return $http.post(BASE_URL + '/search?programUUID=' + programUUID, searchRequest)
					.then(successHandler, failureHandler);
			}
			return service;
		}
	]);
})();
