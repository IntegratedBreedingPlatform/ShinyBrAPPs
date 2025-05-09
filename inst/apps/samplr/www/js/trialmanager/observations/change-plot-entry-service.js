(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.factory('ChangePlotEntryService', ['$http', 'serviceUtilities', 'studyContext',
		function ($http, serviceUtilities, studyContext) {
			var successHandler = serviceUtilities.restSuccessHandler,
				failureHandler = serviceUtilities.restFailureHandler;

			var BASE_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId + '/studies/' + studyContext.studyId;

			var service = {};

			service.updateObservationUnitsEntry = function (observationUnitEntryReplaceRequest, datasetId) {
				const url = BASE_URL + `/datasets/${datasetId}/observation-units/entries`;
				return $http.post(url, observationUnitEntryReplaceRequest)
					.then(successHandler, failureHandler);
			};

			service.getColumns = function () {
				var request = $http.get(BASE_URL + '/entries/table/columns', {
				});
				return request.then(successHandler, failureHandler);
			};

			service.getEntriesTableUrl = function () {
				return BASE_URL + '/entries';
			};

			return service;

		}

	]);
})();
