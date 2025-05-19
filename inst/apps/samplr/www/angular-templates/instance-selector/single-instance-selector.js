(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.directive('singleInstanceSelector', ['DTOptionsBuilder', 'DTColumnBuilder',
		function (DTOptionsBuilder, DTColumnBuilder) {

			return {
				restrict: 'E',
				// require: '^parent',
				scope: {
					instances: '=',
					onSelectInstance: '=',
					selected: '=',
					instanceIdProperty: '@'
				},
				templateUrl: '/Fieldbook/static/angular-templates/instance-selector/single-instance-selector.html',
				controller: function ($scope) {
					$scope.dtOptions = DTOptionsBuilder.newOptions().withDOM('<"row"<"col-sm-6"l><"col-sm-6"f>>' +
						'<"row"<"col-sm-12"tr>>' +
						'<"row"<"col-sm-5"i><"col-sm-7">>' +
						'<"row"<"col-sm-12"p>>');
				}
			};
		}]);
})();
