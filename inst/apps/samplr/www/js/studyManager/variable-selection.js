/* global Handlebars, showErrorMessage,showAlertMessage */

/**
 * @module variable-selection
 */
 var BMS = window.BMS;

if (typeof (BMS) === 'undefined') {
	BMS = {};
}

if (typeof (BMS.NurseryManager) === 'undefined') {
	BMS.NurseryManager = {};
}

BMS.NurseryManager.VariableSelection = (function($) {
	'use strict';

	var VARIABLE_SELECT_EVENT = 'variable-select',
		MODAL_SELECTOR = '.vs-modal',
		TREATMENT_FACTOR_GROUP = 1809,

		modalHeaderSelector = '.modal-header',
		variableNameContainerSelector = '.vs-variable-name-container',
		addVariableButtonSelector = '.vs-variable-select',
		aliasVariableButtonSelector = '.vs-alias-edit',
		aliasVariableInputSelector = '.vs-alias-input',
		relatedPropertyLinkSelector = '.vs-related-prop-name',
		propertyContainerSelector = '.vs-property-container',
		relatedPropertyListSelector = '.vs-related-props-container',
		propertySelectSelector = '.vs-ps-container',

		// Only compile our templates once, rather than every time we need them
		generatePropertyVariableList = Handlebars.compile($('#vs-property-template').html()),
		generateVariableName = Handlebars.compile($('#vs-variable-name-template').html()),
		generateVariableAlias = Handlebars.compile($('#vs-alias-edit-template').html()),
		generateRelatedProperty = Handlebars.compile($('#related-prop-template').html()),

		VariableSelection;

	Handlebars.registerPartial('variable-name', $('#vs-variable-name-partial').html());

	/* FIXME - this logic should be in the back end
	 *
	 * Ensures that variables are converted to their necessary equivalents if required.
	 *
	 * @param {string} variableId the id of the variable to convert
	 * @returns {string} the variable id that has been converted if necessary
	 */
	function _convertVariableId(variableId) {

		// These three variables need special handling - their IDs must be translated before sending to the server
		var idTranslations = {
				// Collaborator
				8373: 8372,
				// PI Name
				8100: 8110,
				// Location
				8180: 8190
			};

		// Check to see if the id is in our list of necessary translations. If it is, return the translation, otherwise return itself.
		return idTranslations[variableId] ? idTranslations[variableId] : variableId;
	}

	/*
	 * Attaches the specified list of related properties to the provided container, after ensuring the currently selected property is
	 * removed from the list.
	 *
	 * @param {JQuery} container the container to which the properties should be appended
	 * @param {object} selectedProperty the currently selected property, which should be removed from the list of related properties
	 * @param {object[]} the list of properties related to the selected property
	 */
	function _renderRelatedProperties(container, selectedProperty, properties) {

		// Filter out the currently selected property
		var filteredProperties = $.grep(properties, function(element) {
				return element.propertyId !== selectedProperty.propertyId;
			}),

			splitColumns = {
				column1: filteredProperties,
				column2: [],
				className: selectedProperty.classesStr
			};

		// Split the list of filtered properties in two if there are more than 4
		if (splitColumns.column1.length > 4) {
			splitColumns.column1 = filteredProperties.splice(0, Math.ceil(filteredProperties.length / 2));
			splitColumns.column2 = filteredProperties;
		}

		container.append(generateRelatedProperty(splitColumns));
	}

	/*
	 * Renders a variable.
	 *
	 * @param {object} variable the variable to render
	 * @param {JQuery} container the container into which the variable should be rendered
	 */
	function _renderVariableName(variable, variableContainer) {

		variableContainer.off('click');
		variableContainer.empty();

		variableContainer.append(generateVariableName(variable));
	}

	/* Constructs a new property dropdown.
	 *
	 * @param {string} placeholder the placeholder to use in the select
	 * @param {object[]} the list of properties to render in the dropdown
	 * @param {function} onSelectingFn a function to perform when the user selects a new property
	 */
	function _instantiatePropertyDropdown(placeholder, properties, onSelectingFn) {

		var propertyDropdown = new window.BMS.NurseryManager.PropertySelect(propertySelectSelector,
			placeholder, properties, onSelectingFn);

		return propertyDropdown;
	}

	/*
	 * Finds a variable with a specified name from a list of variables.
	 *
	 * @param {string} variableName the name fo the variable to find
	 * @param {object[]} variableList the array of variables to search through
	 * @returns {object} an object with two properties, index - the index of the variable, and variable, the variable itself. Returns null
	 * if the element is not found
	 */
	function _findVariableByName(variableName, variableList) {

		var index = -1,
			selectedVariable;

		$.each(variableList, function(i, variableObj) {

			if (variableObj.name === variableName) {
				selectedVariable = variableObj;
			}
			index = i;
			return !selectedVariable;
		});

		// Return null if we failed to find the variable
		return index === -1 ? null : {
			index: index,
			variable: selectedVariable
		};
	}

	/*
	 * Removes an item by item from a given list.
	 *
	 * @param {string} id the id of the item to remove
	 * @param {object[]} list the list of objects to search
	 */
	function _removePropertyById(id, list) {

		// Use each instead of grep to prevent the need to continue to iterate over the array once we've found what we're looking for
		$.each(list, function(index, obj) {

			var found = false;

			if (obj.propertyId === id) {
				found = true;
				list.splice(index, 1);
			}
			return !found;
		});
	}

	/*
	 * Removes properties that should be excluded from a specified list.
	 *
	 * @param {object[]} relatedPropertyList the list of properties to remove excluded items from
	 * @param {object[]} toExclude the list of properties to remove
	 */
	function _removeExclusions(relatedPropertyList, toExclude) {
		$.each(toExclude, function(i, property) {
			_removePropertyById(property.propertyId, relatedPropertyList);
		});
	}

	/**
	 * Creates a new Variable Selection dialog.
	 *
	 * @constructor
	 * @param {string} translations.label the title of the dialog
	 * @param {string} translations.uniqueVariableError error message when a variable name is not unique
	 * @param {string} translations.generalAjaxError error message to display when an ajax request fails
	 */
	VariableSelection = function(translations) {
		this._$modal = $(MODAL_SELECTOR);
		this._translations = translations;
		this._relatedProperties = [];

		this._$modal.on('hide.bs.modal', $.proxy(this._clear, this));
	};

	/**
	 * Display the variable selection dialog for the specified group.
	 *
	 * @param {number} group properties and variables will be filtered to be specific to the group represented by this number
	 * @param {object} translations internationalised labels to be used in the dialog, specific to the group being shown
	 * @param {string} translations.label the title of the dialog
	 * @param {string} translations.placeholderLabel the placeholder in the property dropdown
	 * @param {object} groupData data about the group, including selected variables and properties
	 */
	VariableSelection.prototype.show = function(group, translations, groupData) {

		var properties = groupData.propertyData,
			modalHeader = $(MODAL_SELECTOR + ' ' + modalHeaderSelector),
			title;

		properties.sort(function(propertyA, propertyB) {
			return propertyA.name.localeCompare(propertyB.name);
		});

		// Store these properties for later use
		this._currentlySelectedVariables = groupData.selectedVariables;
		this._group = group;
		this._properties = properties;
		this._callback = groupData.callback;
		this._onHideCallback = groupData.onHideCallback;
		this._excludedProperties = groupData.excludedProperties || [];

		if (groupData.options) {
			var options = groupData.options;
			this._variableSelectBtnName = options.variableSelectBtnName;
			this._variableSelectBtnIco = options.variableSelectBtnIco;
			this._noAlias = options.noAlias;
		}

		this._$modal.one('hidden.bs.modal', $.proxy(this._onHidden, this));

		// Append title
		title = $('<h4 class="modal-title" id="vs-modal-title">' + translations.label + '</h4>');
		modalHeader.append(title);

		// Instantiate property dropdown, passing in a function that will record the new property and load it's details on select
		this._propertyDropdown = _instantiatePropertyDropdown(translations.placeholderLabel, properties, $.proxy(function(e) {
			this._selectedProperty = e.choice;
			this._loadVariablesAndRelatedProperties();
		}, this));

		// TODO Awaiting Rebecca's JSONified variable usage service

		// Listen for variable selection
		$(propertyContainerSelector).on('click', addVariableButtonSelector, {}, $.proxy(function(e) {
			e.preventDefault();
			this._selectVariable($(e.currentTarget));
		}, this));

		// Listen for variable aliasing request
		$(propertyContainerSelector).on('click', aliasVariableButtonSelector, {}, $.proxy(function(e) {
			e.preventDefault();
			var aliasButton = $(e.currentTarget);
			this._aliasVariableButton(aliasButton.parent(variableNameContainerSelector));
		}, this));

		// Listen for the user clicking on a related property
		$(relatedPropertyListSelector).on('click', relatedPropertyLinkSelector, {}, $.proxy(function(e) {
			e.preventDefault();
			this._loadProperty($(e.target).data('id'));
		}, this));

		// Show the modal
		this._$modal.modal({
			backdrop: 'static',
			keyboard: true
		});
	};

	/**
	 * Hide the dialog. This programmatic method is available for use, but because we are using a bootstrap modal, pressing the escape key
	 * will also hide the modal.
	 */
	VariableSelection.prototype.hide = function() {
		this._$modal.modal('hide');
	};

	/**
	 * @returns {JQuery} the modal
	 */
	VariableSelection.prototype.getModal = function() {
		return this._$modal;
	};

	/*
	 * Loads variables and related properties for the currently selected property.
	 */
	VariableSelection.prototype._loadVariablesAndRelatedProperties = function() {

		var selectedProperty = this._selectedProperty,
			variables = this._selectedProperty.standardVariables,
			toExclude = this._excludedProperties,
			propertyVariableList = $(propertyContainerSelector),
			relatedPropertyList = $(relatedPropertyListSelector),
            classesStr = selectedProperty.classesStr,
			selectedVariables = this._currentlySelectedVariables,
			generalAjaxErrorMessage = this._translations.generalAjaxError,

			i,
			selectedVariableName,
			variableId,
			relatedPropertiesKey,
			filteredProperties;

		// If we know of aliases for any of the variables we're loading, set them now
		// DMV : leverage existing loop to set property used for displaying treatment factor specific UI
		for (i = 0; i < variables.length; i++) {
			variableId = variables[i].id;
			selectedVariableName = selectedVariables[variableId];

			if (typeof(selectedVariableName) !== 'undefined') {

				// Only set the alias if it is different from the name we know for the variable
				if (selectedVariableName && selectedVariableName !== variables[i].name) {
					variables[i].alias = selectedVariableName;
				}
				// Whether or not the selected variable name has been provided, if the key is present the variable has
				// been selected
				variables[i].selected = true;
			}

			if (this._group === TREATMENT_FACTOR_GROUP && !variables[i].hasPair) {
				variables[i].showTreatmentFactorValidationMessage = true;
			}
		}

		// Update our saved property list to reflect our new knowledge of aliases and which variables are selected
		this._selectedProperty.standardVariables = variables;

		// Clear out any existing variables and append the variables of the selectedProperty
		propertyVariableList.empty();
		propertyVariableList.append(generatePropertyVariableList({
			propertyName: this._selectedProperty.name,
			className: this._selectedProperty.classesStr,
			variables: variables
		}));

		if (this._variableSelectBtnName) {
			propertyVariableList.find('.vs-variable-select-label').text(this._variableSelectBtnName);
		}

		if (this._variableSelectBtnIco) {
			propertyVariableList.find('.vs-variable-select-icon').switchClass('glyphicon-plus', this._variableSelectBtnIco);
		}

		if (this._noAlias) {
			propertyVariableList.find('.vs-alias-edit').remove();
		}

		// Clear out any existing related properties, and update the related property class name
		relatedPropertyList.empty();

		// Key identifies whether we have retrieved the related properties for this group / class before (so we don't retrieve them again)
		relatedPropertiesKey = this._group + ':' + classesStr;

		if (!this._relatedProperties[relatedPropertiesKey]) {
			var classReqStr = '';
			$.each(selectedProperty.classes, function(key, val) {
				classReqStr += '&classes=' + val;
			});

			var groupId = this._group,
				url = '/Fieldbook/manageSettings/settings/properties?type=' + groupId + classReqStr + '&useTrialFiltering=true';
			$.getJSON(url, $.proxy(function(data) {

				// Check we have not moved on by the time the call returns
				if (this._selectedProperty.propertyId === selectedProperty.propertyId) {
					data.sort(function(propertyA, propertyB) {
						return propertyA.name.localeCompare(propertyB.name);
					});

					// Store for later to prevent multiple calls to the same service with the same data
					this._relatedProperties[relatedPropertiesKey] = data;

					// Remove exclusions but don't modify saved data
					filteredProperties = JSON.parse(JSON.stringify(data));
					_removeExclusions(filteredProperties, toExclude);

					_renderRelatedProperties(relatedPropertyList, selectedProperty, filteredProperties, toExclude);
				}
			}, this)).fail(function(jqxhr, textStatus, error) {

				var errorMessage;

				showErrorMessage(null, generalAjaxErrorMessage);

				if (console) {
					errorMessage = textStatus + ', ' + error;
					console.error('Request to get properties for group ' + groupId + ' and class ' + classId + ' failed with error: ' +
						errorMessage);
				}
			});
		} else {
			// Remove exclusions but don't modify saved data
			filteredProperties = JSON.parse(JSON.stringify(this._relatedProperties[relatedPropertiesKey]));
			_removeExclusions(filteredProperties, toExclude);

			_renderRelatedProperties(relatedPropertyList, selectedProperty, filteredProperties, toExclude);
		}
	};

	/*
	 * Selects a variable.
	 *
	 * @param {JQuery} selectButton the button element that was clicked on
	 * @fires VariableSelection#variable-select
	 */
	VariableSelection.prototype._selectVariable = function(selectButton) {

		var container = selectButton.parent('.vs-variable-select-container'),
			iconContainer = selectButton.children('.glyphicon'),
			generalErrorMessage = this._translations.generalAjaxError,
			variableSelectedMessage = this._translations.variableSelectedMessage,
			variableName,
			selectedVariable,
			variableId,
			callback = this._callback;

		// If the user is in the middle of entering an alias, close that before proceeding
		if (container.find(aliasVariableInputSelector).length) {
			this._saveAlias(container.children(variableNameContainerSelector));
			return;
		}

		variableName = $(container.find('.vs-variable-name')[0]).text();
		selectedVariable = _findVariableByName(variableName, this._selectedProperty.standardVariables).variable;

		if (!selectedVariable) {
			showErrorMessage(null, generalErrorMessage);
			if (console) {
				console.error('Failed to find variable with name \'' + variableName + '\' in list of variables on property with id \' ' +
					this._selectedProperty.propertyId + '\'.');
			}
		}

		// validate alias that come from ontology too
		if (!this._validateAlias(selectedVariable.alias)) {
			return;
		}

		variableId = _convertVariableId(selectedVariable.id);

		var promise;
		if (callback) {
			selectedVariable.cvTermId = variableId;

			var responseData = [{variable: selectedVariable}];
			callback({
				responseData: responseData
			});
			promise = $.Deferred().resolve(responseData).promise();

		} else {
			promise = $.ajax({
				url: '/Fieldbook/manageSettings/addSettings/' + this._group,
				type: 'POST',
				data: JSON.stringify({
					selectedVariables: [{cvTermId: variableId, name: selectedVariable.alias || selectedVariable.name}]
				}),
				dataType: 'json',
				headers: {
					Accept: 'application/json',
					'Content-Type': 'application/json'
				},
				error: function (jqxhr, textStatus, error) {

					var errorMessage;

					showErrorMessage(null, ajaxGenericErrorMsg);
					if (console) {
						errorMessage = textStatus + ', ' + error;
						console.error('Failed to add variable with id ' + variableId + '. Error was: ' + errorMessage);
					}
				}
			}).done(function (data) {
				if (data[0] && data[0].variable.dataTypeId === 1130 &&  data[0].variable.widgetType === 'DROPDOWN' && data[0]
					.possibleValues.length === 0) {
					showAlertMessage('', variableNoValidValueNotification);
				}

				return data
			});
		}
		promise.done($.proxy(function (data) {

			// remove the click functionality to avoid selecting twice
			selectButton.off('click');
			selectButton.on('click', function () {
				showAlertMessage('', variableSelectedMessage);
			});
			selectButton.attr('title', variableSelectedMessage);
			selectButton.removeClass('vs-variable-select');
			selectButton.addClass('vs-variable-button');

			// Prevent this variable from being selected again
			this._currentlySelectedVariables[selectedVariable.id] = selectedVariable.alias || selectedVariable.name;

			// Remove the edit button
			selectButton.parents('.vs-variable').find(aliasVariableButtonSelector).remove();

			// Change the add button to a tick to indicate success
			iconContainer.removeClass('glyphicon-plus').addClass('glyphicon-ok');
			selectButton.children('.vs-variable-select-label').text('');

			/**
			 * Variable select event.
			 *
			 * @event VariableSelection#variable-select
			 * @type {object}
			 * @property {number} group the group the variable belongs to
			 * @property {object} responseData data returned from a successful call to /Fieldbook/manageSettings/addSettings/ or callback
			 */

			this._$modal.trigger({
				type: VARIABLE_SELECT_EVENT,
				group: this._group,
				responseData: data
			});

			/**
			 * Remove variable from VariableCache
			 * across all BMS applications
			 * It's straightforward to do this here
			 * while doing it at the moment of saving
			 * could be a bit more tricky
			 *
			 */
			var authParams =
				'authToken=' + authToken
				+ '&selectedProjectId=' + selectedProjectId
				+ '&loggedInUserId=' + loggedInUserId;

			var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;

			$.each(
				['/bmsapi/crops/' + cropName + '/variable-cache/' + variableId + '?programUUID=' + currentProgramId,
					'/ibpworkbench/controller/' + 'variableCache/' + variableId + '?' + authParams],
				function (i, v) {
					$.ajax({
						url: v,
						type: 'DELETE',
						beforeSend: function(xhr) {
							xhr.setRequestHeader('X-Auth-Token', xAuthToken);
						},
						error: function(jqxhr, textStatus, error) {
							if (jqxhr.status == 401) {
								bmsAuth.handleReAuthentication();
							}
						}
					});
				});
		}, this));

	};

	/*
	 * Handles a variable alias event. Allows the user to provide an alias for a variable name.
	 *
	 * @param {JQuery} container the container of the variable to be aliased
	 */
	VariableSelection.prototype._aliasVariableButton = function(container) {

		var variableName = $(container.children('.vs-variable-name')[0]).text(),
			generalErrorMessage = this._translations.generalAjaxError,
			variableInfo;

		variableInfo = _findVariableByName(variableName, this._selectedProperty.standardVariables);

		if (!variableInfo) {
			showErrorMessage(null, generalErrorMessage);
			if (console) {
				console.error('Failed to find variable with name \'' + variableName + '\' in list of variables on property with id \' ' +
					this._selectedProperty.propertyId + '\'.');
			}
		}

		// Remove the display of the name and edit button, and render the input and save/ cancel buttons
		container.empty();
		container.append(generateVariableAlias({
			index: variableInfo.index,
			alias: variableInfo.variable.alias || ''
		}));

		container.on('click', '.vs-alias-save', {}, $.proxy(function(e) {
			e.preventDefault();
			this._saveAlias($(e.currentTarget).parent(variableNameContainerSelector));
		}, this));

		container.on('click', '.vs-alias-cancel', {}, $.proxy(function(e) {
			e.preventDefault();
			this._cancelAlias($(e.currentTarget).parent(variableNameContainerSelector));
		}, this));

		container.on('keyup ', aliasVariableInputSelector, {}, $.proxy(function(e) {

			var container = $(e.currentTarget).parent(variableNameContainerSelector);

			switch (e.keyCode) {
				case 13:
					// Save on enter
					e.preventDefault();
					this._saveAlias(container);
					break;
				case 27:
					// Cancel on escape - this is actually being trapped by the escape to escape from
					// the modal, so won't work at the moment :(
					e.preventDefault();
					this._cancelAlias(container);
					break;
				default:
					// Don't do anything for any other keys
					break;
			}
		}, this));

		container.find(aliasVariableInputSelector).focus();
	};

	/*
	 * Handles a variable alias save.
	 *
	 * @param {JQuery} container the variable container that holds the input
	 * @returns {string} the new variable name (which will be the alias or the variable name if the alias was empty), or null if an error
	 * occurred
	 */
	VariableSelection.prototype._saveAlias = function(container) {

		var input = container.find(aliasVariableInputSelector),
			alias = input.val(),
			index = input.data('index');

		if (alias) {
			if (!this._validateAlias(alias)) {
				// Don't close the input before returning
				return null;
			}

			// Store the alias
			this._selectedProperty.standardVariables[index].alias = alias;
		}

		_renderVariableName(this._selectedProperty.standardVariables[index], container);

		// Select this variable. It's unlikely the user wanted to add an alias but not use the variable.
		this._selectVariable(container.next(addVariableButtonSelector));

		return alias || this._selectedProperty.standardVariables[index].name;
	};

	VariableSelection.prototype._validateAlias = function (alias) {

		var aliasValidation = new RegExp(/^[a-zA-Z_%]{1}[a-zA-Z_%0-9]{0,31}$/);

		if (alias) {

			alias = alias.trim();

			// Validate alias has no more than 32 characters, starts with a letter, underscore or % sign, and only contains
			// numbers, letters, _ or %
			if (!aliasValidation.test(alias)) {
				showErrorMessage(null, this._translations.invalidAliasError);
				return false;
			}

			// Validate alias is unique among selected variables
			var notUnique = Object.values(this._currentlySelectedVariables).some(function (variableName) {
				return alias === variableName;
			});

			if (notUnique) {
				showErrorMessage(null, this._translations.uniqueVariableError);
				return false;
			}
		}

		return true;
	};

	/*
	 * Cancels editing a variable alias.
	 *
	 * @param {JQuery} container the variable container that holds the input
	 */
	VariableSelection.prototype._cancelAlias = function(container) {

		var input = container.find(aliasVariableInputSelector),
			index = input.data('index');

		_renderVariableName(this._selectedProperty.standardVariables[index], container);
	};

	/*
	 * Loads the selected property.
	 *
	 * @param {string} propertyId the id of the property to load.
	 */
	VariableSelection.prototype._loadProperty = function(propertyId) {

		var property,
			generalErrorMessage = this._translations.generalAjaxError;

		// Find the property that was selected from our list of properties
		$.each(this._properties, function(index, propertyObj) {

			if (propertyObj.propertyId === propertyId) {
				property = propertyObj;
			}

			return !property;
		});

		if (!property) {
			showErrorMessage('', generalErrorMessage);
			if (console) {
				console.error('Failed to load property with id ' + propertyId + '. Could not find it in the list of available properties.');
			}
			return;
		}

		// Set the selected property in the dropdown, store for later use and load the variables and related properties for that property
		this._propertyDropdown.setValue(property);
		this._selectedProperty = property;
		this._loadVariablesAndRelatedProperties();
	};

	/*
	 * Clears out data from the dialog.
	 */
	VariableSelection.prototype._clear = function() {

		var modalHeader = $(MODAL_SELECTOR + ' ' + modalHeaderSelector),
			variableList = $(propertyContainerSelector),
			relatedPropertyList = $(relatedPropertyListSelector);

		// Clear title
		modalHeader.empty();

		// Clear variable and related property selection events
		variableList.off('click');
		relatedPropertyList.off('click');

		// Clear variables and related properties
		variableList.empty();
		relatedPropertyList.empty();

		// Destroy the select dropdown
		this._propertyDropdown.destroy();
	};

	VariableSelection.prototype._onHidden = function() {
		if (this._onHideCallback) {
			this._onHideCallback();
		}
	};

	return VariableSelection;

})(jQuery);
