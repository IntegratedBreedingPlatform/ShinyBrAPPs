/* global angular */
(function() {
	'use strict';

	angular.module('leafnode-utils', [])
		.directive('jqDatepicker', function() {
			return {
				require: '^?ngModel',
				link: function(scope, el, attr, ngModel) {
					if (!ngModel) {
						$(el).datepicker({
							format: 'yyyy-mm-dd'
						});
						return;
					}

					$(el).datepicker({
						format: 'yyyy-mm-dd'
					}).on('changeDate', function() {
						scope.$apply(function() {
							ngModel.$setViewValue(el.val());
						});
						$(this).datepicker('hide');
					});

					ngModel.$render = function() {
						$(el).datepicker('setDate', ngModel.$viewValue);
					};

					if (attr.withImage === 'true') {
						var labelElement = angular.element('<label class="btn"></label>');
						var imageElement = angular.element('<img style="padding-bottom:3px;" src="' + attr.imageSrc + '"/>');

						imageElement.on('click', function() {
							$(el).datepicker('show');
						});

						labelElement.html(imageElement);
						$(el).parent().append(labelElement);
					}
				}
			};
		})
		.directive('slideToggle', function() {
			return {
				link: function(scope, el, attr) {
					$(el).click(function() {
						$(el).find('.icn.section-expanded').toggle();
						$(el).find('.icn.section-collapsed').toggle();

						$(attr.section).slideToggle();
					});
				}
			};
		});

})();

(function() {
	'use strict';
	var getInheritedData = function(element, names) {
		// If element is the document object work with the html element instead
		// this makes $(document).scope() possible
		if (element[0].nodeType === 9) {
			element = angular.element('html');
		}

		var value;
		while (element.length) {
			for (var i = 0, ii = names.length; i < ii; i++) {
				if ((value = element.data(names[i])) !== undefined) {return value;}
			}

			// If dealing with a document fragment node with a host element, and no parent, use the host
			// element as the parent. This enables directives within a Shadow DOM or polyfilled Shadow DOM
			// to lookup parent controllers.
			var node = element[0];
			element = angular.element(node.parentNode || (node.nodeType === 11 && node.host));
		}
	};

	angular.module('leafnode-utils').directive('ngMultiTemplate', [
		function() {
			return {
				controller: function() {
					this.ngMultiTransclude = null;
				},
				templateUrl: function($element, $attrs) {
					return $attrs.ngMultiTemplate;
				},
				transclude: true
			};
		}
	]).directive('ngMultiTranscludeController', [
		function() {
			return {
				controller: function() {
					this.ngMultiTransclude = null;
				}
			};
		}
	]).directive('ngMultiTransclude', [
		function() {
			return {
				link: function(scope, element, attrs, unusedCtrls, transcludeFn) {
					// Ensure we're transcluding or nothing will work.
					if (!transcludeFn) {
						throw new Error(
							'ngMultiTransclude')('orphan',
								'Illegal use of ngMultiTransclude directive in the template! ' +
								'No parent directive that requires a transclusion found. '
						);
					}

					// Find the controller that wraps related multi-transclusions.
					var ctrl = getInheritedData(element, [
						'$ngMultiTranscludeControllerController',
						'$ngMultiTemplateController'
					]);

					if (!ctrl) {
						throw new Error(
								'Illegal use of ngMultiTransclude directive in the template! ' +
								'No parent directive that defines a multi-transclusion controller found. '
						);
					}

					// Replace this element's HTML with the correct
					// part of the clone.
					var attach = function(clone) {
						var el;
						for (var i = 0; i < clone.length; i++) {
							el = angular.element(clone[i]);

							// Uses the argument as the `name` attribute directly, but we could
							// evaluate it or interpolate it or whatever.
							if (el.attr('name') === attrs.ngMultiTransclude) {
								element.append(el);
								return;
							}
						}
					};

					// Only link the clone if we haven't already; store
					// the already-linked clone on the controller so that
					// it can be referenced by all relevant instances of
					// the `ng-multi-transclude` directive.
					if (ctrl.ngMultiTransclude) {
						attach(ctrl.ngMultiTransclude);
					} else {
						transcludeFn(function(clone) {
							ctrl.ngMultiTransclude = clone;
							attach(clone);
						});
					}
				}
			};
		}
	]).directive('sectionContainer', ['$parse', '$http', 'serviceUtilities', 'helpLinkService', function($parse, $http, serviceUtilities, helpLinkService) {
			return {
				restrict: 'E',
				scope: {
					heading: '@',
					reminder: '@',
					helpTooltip: '@',
					icon: '@',
					iconImg: '@',
					iconSize: '@',
					modelData: '=',
					selectedVariables: '=',
					selectVariableCallback: '=',
					onHideCallback: '=',
					variableType: '@',
					showReminder: '=',
					enableUpdate: '=',
					onUpdate: '&',
					callback: '&',
					hideVariable: '=',
					useExactProperties: '@',
					collapsible: '=',
					toggleSection: '=',
					actionButtonDirection: '@',
					helpToolType:'@',
					helpToolUrl:'='
				},
				transclude: true,
				templateUrl: '/Fieldbook/static/angular-templates/sectionContainer.html',
				link: function(scope, elem, attrs) {
					scope.addVariable = $parse(attrs.addVariable)();

					attrs.$observe('helpTooltip', function(value) {
						if (value) {
							scope.hasHelpTooltip = true;
						}
					});

					attrs.$observe('helpToolType', function (value){
						if (value) {
							helpLinkService.helpLink(value).then(function (url){
								scope.helpToolUrl = url;
							});
						}
					});



				},
				controller: ['$scope', '$attrs', function($scope, $attrs) {
					$scope.doCollapse = function() {
						if ($scope.collapsible) {
							$scope.toggleSection = !$scope.toggleSection;
						}
					};

					$scope.doClick = function() {
						$scope.onUpdate({});
					};

					$scope.onAdd = function(result) {
						$scope.callback({ result: result });
					};
				}]

			};
		}]).directive('ontologySummaryTable', ['_', function(_) {
			return {
				restrict: 'E',
				scope: {
					heading: '@',
					propertyTitle: '@',
					valueTitle: '@',
					variableType: '@',
					data: '='
				},
				templateUrl: '/Fieldbook/static/angular-templates/ontologySummaryTable.html',

				// isolated scope values becomes an empty object when passed a null value
				// this compile function will just make scope.data back to null if it has no properties;

				compile: function() {
					return {
						pre: function(scope) {
							if (_.isEmpty(scope.data)) {
								scope.data = null;
							}
						}
					};
				}
			};
		}]).directive('showDetailsModal', function() {
			return {
				scope: {
					showDetailsModal: '=',
					variableType: '@'
				},

				link: function(scope, elem) {
					elem.css({ cursor: 'pointer' });
					elem.on('click', function() {
						showBaselineTraitDetailsModal(scope.showDetailsModal, scope.variableType);
					});
				}
			};
		})

		// CONSTANTS SECTION
		.constant('VARIABLE_TYPES', {
			ANALYSIS: 1801,
			STUDY_CONDITION: 1802,
			GERMPLASM_DESCRIPTOR: 1804,
			STUDY_DETAIL: 1805,
			ENVIRONMENT_DETAIL: 1806,
			SELECTION_METHOD: 1807,
			TRAIT: 1808,
			TREATMENT_FACTOR: 1809,
			EXPERIMENTAL_DESIGN: 1810
		})

		// USAGE: Returns a function, that, as long as it continues to be invoked, will not
		// be triggered. The function will be called after it stops being called for
		// N milliseconds. If `immediate` is passed, trigger the function on the
		// leading edge, instead of the trailing.

		.service('debounce', ['$timeout', function($timeout) {
			return function(func, wait, immediate, invokeApply) {
				var timeout, args, context, timestamp, result;
				function debounce() {
					/* jshint validthis:true */
					context = this;
					args = arguments;
					timestamp = new Date();

					var later = function() {
						var last = (new Date()) - timestamp;
						if (last < wait) {
							timeout = $timeout(later, wait - last, invokeApply);
						} else {
							timeout = null;

							if (!immediate) {
								result = func.apply(context, args);
							}
						}
					};

					var callNow = immediate && !timeout;
					if (!timeout) {
						timeout = $timeout(later, wait, invokeApply);
					}
					if (callNow) {
						result = func.apply(context, args);
					}

					return result;
				}
				debounce.cancel = function() {
					$timeout.cancel(timeout);
					timeout = null;
				};
				return debounce;
			};
		}]);
})();
