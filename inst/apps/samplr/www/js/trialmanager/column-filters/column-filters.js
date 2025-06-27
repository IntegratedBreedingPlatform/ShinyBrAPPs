(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.directive('studyColumnFilterIcon', function () {
		return {
			restrict: 'E',
			templateUrl: 'studyColumnFilterIcon.html',
			scope: {
				studyColumnFilterPopoverTemplate: '@',
				filter: '=',
				columnName: '@',
				filterHelper: '='
			}
		}
	}).directive('studyColumnFilter', function () {
		return {
			resctrict: 'E',
			templateUrl: 'studyColumnFilter.html',
			transclude: true
		}
	});
})();
