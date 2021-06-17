/*global angular*/

(function () {
	'use strict';

	var manageTrialApp = angular.module('manageTrialApp');

	manageTrialApp.controller('SubObservationTabCtrl', ['$scope', 'TrialManagerDataService', '$stateParams','helpLinkService',
		function ($scope, TrialManagerDataService, $stateParams, helpLinkService) {

			$scope.subObservationTab = $stateParams.subObservationTab;
			$scope.title = $scope.subObservationTab.name;
			$scope.tabTitlePrefix = $scope.subObservationTab.datasetType.tabTitlePrefix;
			helpLinkService.helpLink('MANAGE_STUDIES_SUB_OBSERVATIONS').then(function (url) {
				$scope.helpLink = url;
			});

		}]);

})();
