(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.factory('PlantingPreparationService', ['$http', 'serviceUtilities', 'studyContext',
		function ($http, serviceUtilities, studyContext) {
			var successHandler = serviceUtilities.restSuccessHandler,
				failureHandler = serviceUtilities.restFailureHandler;

			var BASE_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId + '/studies/' + studyContext.studyId;

			var service = {};

			service.getPlantingPreparationData = function (searchComposite, datasetId) {
				return $http.post(`${BASE_URL}/datasets/${datasetId}/planting/preparation/search`, searchComposite)
					.then(successHandler, failureHandler);
			};

			service.getMetadata = function (plantingRequest, datasetId) {
				return $http.post(`${BASE_URL}/datasets/${datasetId}/planting/metadata`, plantingRequest)
					.then(successHandler, failureHandler);
			}

			service.confirmPlanting = function (plantingRequest, datasetId, isCommitOnSaving) {
				const url = BASE_URL + `/datasets/${datasetId}/planting/` + (isCommitOnSaving ? 'confirmed-generation' : 'pending-generation');
				return $http.post(url, plantingRequest)
					.then(successHandler, failureHandler);
			};

			return service;
		}
	]);
})();
