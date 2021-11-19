/*global angular*/
'use strict';

(function() {
	var manageTrialApp = angular.module('manageTrialApp');
	manageTrialApp.service('variableService', ['$http', 'serviceUtilities', 'studyContext', function($http, serviceUtilities, studyContext) {

		var successHandler = serviceUtilities.restSuccessHandler,
			failureHandler = serviceUtilities.restFailureHandler;


		return {

			getVariable: function (id) {
				var request = $http.get('/bmsapi/ontology/' + studyContext.cropName + '/variables/' + id + '?programId=' +
					studyContext.programId);
				return request.then(successHandler, failureHandler);
			},

			getVariables: function () {
				var request = $http.get('/bmsapi/ontology/' + studyContext.cropName + '/variables?programId=' +
					studyContext.programId);
				return request.then(successHandler, failureHandler);
			},

			getVariablesByFilter: function (filter) {
				/**
				 * See org.ibp.api.rest.ontology.VariableFilterResource.listAllVariablesUsingFilter
				 */
				var request = $http.get('/bmsapi/crops/' + studyContext.cropName + '/variables/filter', angular.merge({
					params: angular.merge({
						programUUID: studyContext.programId
					}, filter)
				}));
				return request.then(successHandler, failureHandler);
			}
		};
	}]);

}());
