(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.directive('singleInstanceSelectorModal', [
		function () {
			return {
				restrict: 'E',
				scope: {
					instances: '=',
					onSelectInstance: '=',
					selected: '=',
					instanceIdProperty: '@',
					onContinue: '='
				},
				templateUrl: '/Fieldbook/static/angular-templates/instance-selector/single-instance-selector-modal.html',
				controller: function ($scope) {
					$scope.cancel = function () {
						$scope.$parent.$dismiss();
					};

					$scope.continue = function () {
						$scope.onContinue();
						$scope.$parent.$close();
					};
				}
			}
		}
	]);

})();
