(function () {
	'use strict';

	const module = angular.module('auth');

	module.factory('AccountService', ['$http', 'serviceUtilities', 'studyContext',
		function ($http, serviceUtilities, studyContext) {
			var successHandler = serviceUtilities.restSuccessHandler,
				failureHandler = serviceUtilities.restFailureHandler;

			var service = {};

			service.get = function () {
				return $http.get('/bmsapi/account', {
					params: {
						cropName: studyContext.cropName,
						programUUID: studyContext.programId
					}
				}).then(successHandler, failureHandler);
			}

			return service;
		}
	]);
})();
