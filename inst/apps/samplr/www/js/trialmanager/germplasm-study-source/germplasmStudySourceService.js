(function () {
	'use strict';

	var germplasmStudySourceModule = angular.module('germplasm-study-source');

	germplasmStudySourceModule.factory('germplasmStudySourceService', ['$http', '$q', 'studyContext', 'serviceUtilities',
		function ($http, $q, studyContext, serviceUtilities) {

			var BASE_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId + '/studies/';
			var successHandler = serviceUtilities.restSuccessHandler,
				failureHandler = serviceUtilities.restFailureHandler;

			var germplasmStudySourceService = {};

			germplasmStudySourceService.getGermplasmStudySourceTableUrl = function () {
				return BASE_URL + studyContext.studyId + '/germplasm-sources/table';
			};

			germplasmStudySourceService.searchGermplasmStudySources = function (germplasmStudySourceRequest, page, pageSize) {
				germplasmStudySourceRequest.studyId = studyContext.studyId;
				return $http.post(BASE_URL + studyContext.studyId + '/germplasm-sources/table' + ((page && pageSize) ? '?page=' + page + '&size=' + pageSize : ''), germplasmStudySourceRequest)
					.then(successHandler, failureHandler);
			};

			return germplasmStudySourceService;

		}]);

})();
