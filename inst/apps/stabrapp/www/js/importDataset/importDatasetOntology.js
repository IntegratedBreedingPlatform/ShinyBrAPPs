(function () {
	'use strict';

	angular.module('di-mapHeaders', ['digitalabs-util', 'dataset-importer-directives', 'digitalabs-data', 'ui.bootstrap', 'ui.select2', 'ui.sortable', 'LocalStorageModule', 'dataset-importer-directives'])
		.config(['localStorageServiceProvider', function (localStorageServiceProvider) {
			localStorageServiceProvider.setPrefix('etl-onto-mapping');
			localStorageServiceProvider.setStorageType('sessionStorage');
		}])
		.constant('APPLICATION_BASE', applicationBase)
		.constant('FIELDBOOK_WEB_LINK', fieldbookWebLink)
		.controller('MapHeadersJSController', ['$scope', '$modal', 'Utilities', 'StandardVariables', 'myHttp', 'APPLICATION_BASE', 'FIELDBOOK_WEB_LINK', function ($scope, $modal, Utilities, StandardVariables, myHttp, APPLICATION_BASE, FIELDBOOK_WEB_LINK) {

			$scope.mappingdata = {};
			$scope.mappingdata.unmatched = headerList;
			$scope.mappingdata.trialEnvironment = trialEnvironmentList;
			$scope.mappingdata.germplasm = germplasmList;
			$scope.mappingdata.trialDesign = trialDesignList;
			$scope.mappingdata.variate = variateList;
			$scope.serverMessages = [];
			$scope.errorMessages = {};
			$scope.errorMessages.CONTAINS_UNMAPPED = errorUnmappedVariables;

			$scope.missingEntryOrPlot = false;
			$scope.missingTrial = false;
			$scope.unmappedPresent = false;

			$scope.advancedOptions = {
				showAdvancedOptions: false,
				maintainHeaderNaming: false
			};

			$scope.uiSortOpts = {

				connectWith: '.list-group',
				cancel: '[data-empty="true"]',
				/*handle : "div[data-empty='false']"*/
				/*start: function(e, ui) {
				 if (ui.item[0].innerText == 'None') {
				 ui.item.sortable.cancel();
				 }
				 },*/
				receive: function (e, ui) {
					// utilizes customization of multisortable.js to manipulate target object and clear it of standard variable data, leaving only header name and phenotype
					// TODO : move logic for proper update of phenotype value from Re-Map button to here
					ui.targetObject.id = null;
					ui.targetObject.variable = null;
					ui.targetObject.property = null;
					ui.targetObject.scale = null;
					ui.targetObject.method = null;
					ui.targetObject.propertyId = null;
					ui.targetObject.scaleId = null;
					ui.targetObject.methodId = null;
				}
			};

			$scope.uiSortOptsExt = {

				connectWith: '.list-group',
				cancel: '[data-empty="true"]',
				/*handle : "div[data-empty='false']"*/
				/*start: function(e, ui) {
				 if (ui.item[0].innerText == 'None') {
				 ui.item.sortable.cancel();
				 }
				 },*/
				receive: function (e, ui) {
					// utilizes customization of multisortable.js to manipulate target object and clear it of standard variable data, leaving only header name and phenotype
					// TODO : move logic for proper update of phenotype value from Re-Map button to here
					ui.targetObject.id = null;
					ui.targetObject.variable = null;
					ui.targetObject.property = null;
					ui.targetObject.scale = null;
					ui.targetObject.method = null;
					ui.targetObject.propertyId = null;
					ui.targetObject.scaleId = null;
					ui.targetObject.methodId = null;

					$scope.performRemap(ui.item.scope().header, e.target.getAttribute("data-phenotype"));
				}
			};


			$scope.previewMode = false;

			$scope.$watch('previewMode', function () {
				window.scrollTo(window.scrollX, window.scrollY + 1);

				window.scrollTo(window.scrollX, window.scrollY - 1);
			});

			$scope.hasErrors = false;

			$scope.data = {};

			// Lets store this in local sessionstorage first, if possible we can call this localStorage BEFORE the mapping page
			StandardVariables.retrieveAll().then(function (response) {
				$scope.data = response;
				$scope.isReady = true;
			});

			$scope.launchOntologyBrowser = function (type, variable) {
				var modalInstance = null;
				var title = "Ontology Browser";
				var url = "/ibpworkbench/controller/ontology";
				modalInstance = $modal.open({
					windowClass: 'modal-very-huge',
					templateUrl: 'ontologyBrowser.html',
					controller: 'OntologyBrowserController',
					resolve: {
						title: function () {
							return title;
						},

						url: function () {
							return url;
						}
					}
				})


			};

			$scope.performRemap = function (currentHeader, phenotypicType) {
				var modalInstance = null;
				currentHeader.phenotype = phenotypicType;

				modalInstance = $modal.open({
					windowClass: 'modal-huge',
					templateUrl: 'remapModal.html',
					controller: 'RemapModalController',
					resolve: {
						currentHeader: function () {
							return currentHeader;
						},

						changeMappingFunction: function () {
							return $scope.performChangePhenotypicMapping;
						},

						checkForStdVarDuplicatesFunction: function () {
							return $scope.checkForStdVarDuplicates;
						}
					}
				});
			};

			$scope.computeButtonLabel = function (header) {
				if (header.variable) {
					return 'Re-map';
				} else {
					return 'Apply Mapping';
				}
			};

			$scope.toggleAdvancedOptions = function () {
				$scope.advancedOptions.showAdvancedOptions = !$scope.advancedOptions.showAdvancedOptions;
			};

			$scope.performValidateHeaderMapping = function () {
				// clear error highlights
				$.each($scope.mappingdata, function (key, variableArray) {
					$.each(variableArray, function (key, variable) {
						variable.hasError = false;
					});
				});

				$scope.unmappedPresent = false;

				$scope.userAttemptedSubmission = true;

				$scope.serverMessages = [];

				var postData = $scope.mappingdata.germplasm.concat($scope.mappingdata.trialDesign).concat($scope.mappingdata.trialEnvironment).concat($scope.mappingdata.variate);

				myHttp.post(APPLICATION_BASE + '/etl/workbook/mapOntology', postData).then(function (response) {
					$scope.previewMode = true;

					// this iterates over
					$.each(response.data, function (key, messageList) {
						$.each(messageList, function (index, value) {
							if ($.inArray(value, $scope.serverMessages) == -1) {
								$scope.serverMessages.push(value);
							}
						});

						if (key == 'MISSING_TRIAL') {
							$scope.missingTrial = true;
						} else if (key.indexOf("MISSING") != -1) {
							$scope.missingEntryOrPlot = true;
						} else {

							// implement logic to display message / error for specific header
							// NOTE : key in this case will contain the name of the specific header with problem

							var headerKey = key;

							$.each($scope.mappingdata, function (key, variableArray) {
								$.each(variableArray, function (key, variable) {

									if (variable.id == headerKey.split(':')[1]) {
										variable.hasError = true;
									}
								});

							});
						}
					});

					if ($scope.serverMessages.length != 0) {
						$scope.hasErrors = true;
					}

					$scope.userAttemptedSubmission = false;
				}, function (response) {
					// implement error handling logic in case of communication problem with server
					$scope.userAttemptedSubmission = false;
				});

				if ($scope.hasUnmappedItems()) {
					$scope.unmappedPresent = true;
				}
			};


			// returns to edit mode
			$scope.performRedoHeaderMapping = function () {

				$scope.previewMode = false;
				$scope.hasErrors = false;

			};

			//
			// Do the uploading of header mapping (ASSUMES EVERYTHING IS VALIDATED)
			//
			$scope.performSaveOntologyHeaderMapping = function () {
				$scope.serverMessages = [];

				var postData = $scope.mappingdata.germplasm.concat($scope.mappingdata.trialDesign).concat($scope.mappingdata.trialEnvironment).concat($scope.mappingdata.variate);

				myHttp.post(APPLICATION_BASE + "/etl/workbook/mapOntology/confirm/" + $scope.advancedOptions.maintainHeaderNaming, postData).then(function (response) {
					if (response.data.success) {
						window.location.href = response.data.redirectUrl;
					} else {
						if (response.data.messages) {
							$scope.serverMessages.push(response.data.messages);
							if ($scope.serverMessages.length != 0) {
								$scope.hasErrors = true;
							}
						}
					}
				}, function (response) {
					alert('Problem occurred while sending to server');
				});

			};

			$scope.retrieveMappingData = function (phenotypicType) {
				if (phenotypicType === 'trialEnvironment') {
					return $scope.mappingdata.trialEnvironment;
				} else if (phenotypicType === 'trialDesign') {
					return $scope.mappingdata.trialDesign;
				} else if (phenotypicType === 'variate') {
					return $scope.mappingdata.variate;
				} else if (phenotypicType === 'germplasmEntry') {
					return $scope.mappingdata.germplasm;
				}
			};

			$scope.performChangePhenotypicMapping = function (currentHeader, targetType) {
				var currentPhenoMap = $scope.retrieveMappingData(currentHeader.phenotype);
				var targetPhenoMap = $scope.retrieveMappingData(targetType);

				Utilities.remove(currentPhenoMap, currentHeader);
				targetPhenoMap.push(currentHeader);

			};

			$scope.hasUnmappedItems = function () {
				var counter = 0;

				if ($scope.mappingdata.unmatched.length > 0) {
					return true;
				} else {

					$.each($scope.mappingdata, function (phenotypicType, typeMapping) {
						$.each(typeMapping, function (name, variable) {
							if (!variable.id || variable.id === 0) {
								counter++;
							}
						})
					});

					return counter > 0;
				}
			};


			$scope.checkForStdVarDuplicates = function (variableId, phenotype) {

				var targetPhenoMap = $scope.retrieveMappingData(phenotype);

				//count the headers with duplicate variableId
				var counter = 0;
				$.each(targetPhenoMap, function (key, variable) {
					if (variable.id === variableId) {
						counter++;
					}
				});

				//remove the error highlight if there are no headers with duplicate variableId
				$.each(targetPhenoMap, function (key, variable) {
					if (variable.id === variableId && counter == 2) {
						variable.hasError = false;
					}
				});

			};

			$scope.getOntologySuffix = function (id) {
				if (id === null || typeof id === 'undefined' || id === "") return "";
				return "";
			};

		}])
		.controller('OntologyBrowserController', ['StandardVariables', 'localStorageService', '$scope', '$modalInstance', 'title', 'url',
			function (StandardVariables, localStorageService, $scope, $modalInstance, title, url) {
				$scope.title = title;
				$scope.url = url;
				$scope.close = function () {
					var datasetType = dsType;
					var cachedStandardVariablesName = 'cachedStandardVariables' + datasetType;
					StandardVariables.data.standardVariables.length = 0;
					localStorageService.remove(cachedStandardVariablesName);
					$modalInstance.dismiss('Cancelled');
					//TODO check perfomance here
					StandardVariables.retrieveAll().then(function (response) {
						$scope.data = response;
						$scope.isReady = true;
					});
				};

			}])
		.controller('RemapModalController', ['StandardVariables', '$scope', '$modalInstance', 'currentHeader', 'changeMappingFunction', 'checkForStdVarDuplicatesFunction', 'filterFilter', '$modal', 'FIELDBOOK_WEB_LINK',
			function (StandardVariables, $scope, $modalInstance, currentHeader, changeMappingFunction, checkForStdVarDuplicatesFunction, filterFilter, $modal, FIELDBOOK_WEB_LINK) {
				// store the current header / mapping in a variable local to the scope
				$scope.initialData = currentHeader;
				$scope.messages = [];
				$scope.data = StandardVariables.data;
				$scope.roleList = roleList;

				$scope.userAttemptedSubmission = false;


				// pre populate the selected using values from the passed in parameters.
				$scope.selected = {phenotypicType: currentHeader.phenotype, standardVariable: currentHeader.id};

				// use filter provided by Angular to provide initial filtering of standard variable list
				$scope.data.filteredList = filterFilter($scope.data.standardVariables, {phenotype: $scope.selected.phenotypicType});

				// create and maintain a variable keeping track of user submission attempts. used to control when to display error messages to user
				$scope.userAttemptedSubmission = false;

				$scope.close = function () {
					$modalInstance.dismiss('Cancelled');
				};

				$scope.standardVariableDropdownOptions = {
					data: function () {
						return {results: $scope.data.filteredList}
					},
					initSelection: function (element, callback) {
						callback($scope.initialData);
					},
					formatSelection: function (item) {
						return $scope.getVariableName(item);
					},
					formatResult: function (item, container, query) {
						return $scope.format(item, query)
					},
					query: function (options) {
						return $scope.filterStandardVariables(options)
					}
				};

				$scope.getVariableName = function (item) {
					if(item.alias) {
						return item.variable + " (" + item.alias + ")";
					}
					return item.variable;
				}

				// TODO: NOTE, move the stylings in external CSS later
				$scope.format = function (item, query) {
					var output = '<span style="font-size: 14pt; font-weight: 300" class="text-info">' + $scope.highlightQueryMatch(item.variable, query.term) + '</span>';
					output += '<small class=\"text-info\" >' + $scope.getAlias(item) + '</small>';
					output += '<div>';
					output += '<strong>Property :</strong> ' + $scope.highlightQueryMatch(item.property, query.term);
					output += '<small class=\"text-info\">' + $scope.getOntologySuffix(item.propertyId) + '</small>, ';
					output += '<strong>Scale :</strong> ' + $scope.highlightQueryMatch(item.scale, query.term);
					output += '<small class=\"text-info\">' + $scope.getOntologySuffix(item.scaleId) + '</small>, ';
					output += '<strong>Method :</strong> ' + $scope.highlightQueryMatch(item.method, query.term);
					output += '<small class=\"text-info\">' + $scope.getOntologySuffix(item.methodId) + '</small> ';
					output += '</div>';

					return output;
				};

				$scope.getAlias = function (item) {
					if(item.alias) {
						return " (" + item.alias + ")";
					}
					return "";
				}

				$scope.getOntologySuffix = function (id) {
					if (id === null || typeof id === 'undefined' || id === "") return "";
					return "";
				};

				$scope.highlightQueryMatch = function (value, searchTerm) {
					var index = value.toLowerCase().indexOf(searchTerm.toLowerCase());
					if (index != -1) {
						return value.slice(0, index) + '<b>' + value.slice(index, index + searchTerm.length) + '</b>' + value.slice(index + searchTerm.length);
					} else {
						return value;
					}
				};


				$scope.filterStandardVariables = function (options) {

					var resultList = [];
					var searchTerm = options.term.toLowerCase();

					$.each($scope.data.filteredList, function (index, value) {
						if (value.variable.toLowerCase().indexOf(searchTerm) != -1) {
							resultList.push(value);
						} else if (value.property.toLowerCase().indexOf(searchTerm) != -1) {
							resultList.push(value);
						} else if (value.scale.toLowerCase().indexOf(searchTerm) != -1) {
							resultList.push(value);
						} else if (value.method.toLowerCase().indexOf(searchTerm) != -1) {
							resultList.push(value);
						}
					});

					options.callback({results: resultList, more: false});
				};

				$scope.handlePhenotypeChange = function () {
					// re-evaluate contents of filtered list
					$scope.data.filteredList = filterFilter($scope.data.standardVariables, {phenotype: $scope.selected.phenotypicType});
					$scope.selected.standardVariable = null;
				};

				$scope.save = function () {
					$scope.userAttemptedSubmission = true;

					if ($scope.isValid()) {
						$scope.changePhenotypicMapping();
						$scope.changeStandardVariableMapping();

						$scope.clearCurrentSubmissionAttempt();
						$modalInstance.close($scope.initialData);
					}
				};

				$scope.clearCurrentSubmissionAttempt = function () {
					$scope.userAttemptedSubmission = false;
					$scope.messages = [];
				};

				$scope.isValid = function () {
					var valid = true;

					if (!$scope.selected.phenotypicType) {
						valid = false;
						$scope.messages.push(headerMappingValidationNoPhenotypicType);
					}

					// added check to handle case where selected variabled is "blanked" when header is remapped to different group
					if (!$scope.selected.standardVariable || !$scope.selected.standardVariable.id) {
						valid = false;
						$scope.messages.push(headerMappingValidationNoStandardVariable);
					}

					return valid;
				};

				$scope.changePhenotypicMapping = function () {
					if ($scope.initialData.phenotype !== $scope.selected.phenotypicType) {
						changeMappingFunction($scope.initialData, $scope.selected.phenotypicType);
					}
				};

				$scope.changeStandardVariableMapping = function () {
					var temp = $scope.initialData.headerName;

					var oldMappingId = $scope.initialData.id;
					var oldMappingPhenotype = $scope.initialData.phenotype;
					var oldMappingHasError = $scope.initialData.hasError;

					if ($scope.selected.standardVariable.id !== oldMappingId) {
						checkForStdVarDuplicatesFunction(oldMappingId, oldMappingPhenotype);
					}

					$.extend($scope.initialData, $scope.selected.standardVariable);
					$scope.initialData.headerName = temp;

					if ($scope.initialData.id === oldMappingId) {
						$scope.initialData.hasError = oldMappingHasError;
					}


				};

				$scope.showError = function (ngModelController) {
					return ngModelController.$invalid && $scope.userAttemptedSubmission;
				};

				// ONTOLOGY INTEGRATION CALLBACKS
				$scope.performOntologyVariableUpdate = function (standardVariable) {
					console.log("IM HERE>> $scope.performOntologyVariableUpdate");

					if (standardVariable === undefined || standardVariable.status === undefined) return;
					switch (standardVariable.status) {
						case "ADD":
							standardVariable.phenotype = $scope.getPhenotypeByStoredinID(standardVariable.storedInId);
							StandardVariables.add(standardVariable);
							$scope.data.filteredList = filterFilter($scope.data.standardVariables, {phenotype: $scope.selected.phenotypicType});
							$scope.selected.phenotypicType = standardVariable.phenotype;
							$scope.selected.standardVariable = standardVariable;
							break;
						case "UPDATE":
							StandardVariables.update(standardVariable);
							standardVariable.phenotype = $scope.getPhenotypeByStoredinID(standardVariable.storedInId);
							$scope.selected.standardVariable = null;
							$scope.data.filteredList = filterFilter($scope.data.standardVariables, {phenotype: $scope.selected.phenotypicType});
							$scope.selected.phenotypicType = standardVariable.phenotype;
							$scope.selected.standardVariable = standardVariable;
							break;
						case "DELETE":
							StandardVariables.remove(standardVariable);
							$scope.data.filteredList = filterFilter($scope.data.standardVariables, {phenotype: $scope.selected.phenotypicType});
							$scope.selected.standardVariable = null;
							break;
					}

					// attempt to close modal dialog if exists. this will error otherwise
					try {
						$scope.ontologyManageVarModalInstance.close();
					}
					catch (err) {/* don't care hmmph.. */
					}
				};

				$scope.launchOntologyBrowser = function (variableId) {

					var title = variableId === 0 ? "Add new Standard Variable" : "Update <span class='text-primary'>" + $scope.selected.standardVariable.variable + "</span> Variable";
					var url = FIELDBOOK_WEB_LINK + "/OntologyManager/manage/variable/id/" + variableId; //TODO: hardcoded urls must be moved to the server side and added as tool
					$scope.ontologyManageVarModalInstance = $modal.open({
						windowClass: 'modal-very-huge',
						templateUrl: 'ontologyBrowser.html',
						controller: 'OntologyBrowserController',
						resolve: {
							title: function () {
								return title;
							},

							url: function () {
								return url;
							}
						}
					});
				};

				$scope.getPhenotypeByStoredinID = function (id) {
					var phenotype = null;

					$.each($scope.roleList, function (key, value) {

						if (value.indexOf(parseInt(id)) !== -1) {

							switch (key) {
								case "TRIAL_DESIGN":
									phenotype = "trialDesign";
									break;
								case "GERMPLASM":
									phenotype = "germplasmEntry";
									break;
								case "VARIATE":
									phenotype = "variate";
									break;
								case "TRIAL_ENVIRONMENT":
									phenotype = "trialEnvironment";
									break;
							}
							return false;
						}

					});

					return phenotype;

				}

				var applyOntologyManageVariableUpdate = function (standardVariable, options) {
					console.log("I am called!");

					// access angularJS MapHeadersJSController scope
					if ($("#mappingForm").length > 0)
						window.angular.element("#mappingForm").scope().$parent.$apply(function (_scope) {
							console.log("apply is called");
							console.log(standardVariable);

							_scope.performOntologyVariableUpdate(standardVariable);

						});
				};

				$(document).ready(function () {


				});

			}])
		.factory('StandardVariables', ['resourceFactory', 'localStorageService', '$q', function (resourceFactory, localStorageService, $q) {
			var datasetType = dsType;
			var cachedStandardVariablesName = 'cachedStandardVariables' + datasetType;
			return {
				data: {
					standardVariables: [],

					// define the possible values for phenotypic type. See AppConstants.java for values
					phenotypicTypes: [
						{name: 'Trial Environment', value: 'trialEnvironment'},
						{name: 'Germplasm Entry', value: 'germplasmEntry'},
						{name: 'Trial Design', value: 'trialDesign'},
						{name: 'Variate', value: 'variate'}
					]
				},

				retrieveAll: function () {
					// this fxn always returns a promise
					var deferred = $q.defer();
					var myData = this.data;

					if (this.data.standardVariables.length == 0) {

						this.data.standardVariables = localStorageService.get(cachedStandardVariablesName);

						if (this.data.standardVariables == null || this.data.standardVariables.length == 0) {
							resourceFactory("etl/api/standardVariable/datasetType/" + datasetType).retrieveAll().then(function (response) {
								myData.standardVariables = response;
								localStorageService.add(cachedStandardVariablesName, response);

								deferred.resolve(response);
							});
						} else
							deferred.resolve(this.data);
					} else
						deferred.resolve(this.data);

					return deferred.promise;

				},

				add: function (standardVariable) {
					this.data.standardVariables.push(standardVariable);
					localStorageService.add(cachedStandardVariablesName, this.data.standardVariables);
				},

				update: function (standardVariable) {

					var index = (function (obj, arr) {
						for (var i = 0; i < arr.length; i++) {
							if (arr[i].id === obj.id)
								return i;
						}

						return null;
					}(standardVariable, this.data.standardVariables));

					$.extend(this.data.standardVariables[index], standardVariable);
					localStorageService.add(cachedStandardVariablesName, this.data.standardVariables);


				},

				remove: function (standardVariable) {

					var index = (function (obj, arr) {
						for (var i = 0; i < arr.length; i++) {
							if (arr[i].id === obj.id)
								return i;
						}

						return null;
					}(standardVariable, this.data.standardVariables));

					if (index != null)
						this.data.standardVariables.splice(index, 1);

					localStorageService.add(cachedStandardVariablesName, this.data.standardVariables);
				}
			}
		}]);
}());
