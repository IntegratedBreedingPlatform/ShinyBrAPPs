// FIXME Currently this module depends on globally available select2 code. Should try and fix this.
/* global Handlebars */

/**
 * @module property-select
 */
 var BMS = window.BMS;

if (typeof (BMS) === 'undefined') {
	BMS = {};
}

if (typeof (BMS.NurseryManager) === 'undefined') {
	BMS.NurseryManager = {};
}

BMS.NurseryManager.PropertySelect = (function($) {
	'use strict';

	var generateDropdownMarkup = Handlebars.compile($('#vs-property-select-template').html()),
		generateFormattedResult = Handlebars.compile($('#vs-property-result-template').html()),
		propertyDropdownSelector = '.ps-input',

		PropertySelect;

	Handlebars.registerHelper('searchTermHighlight', function(name, searchTerm) {
		var regex = new RegExp(searchTerm, 'gi');

		if (searchTerm && name) {
			return name.replace(regex, '<span class="ps-item-property-search-term">$&</span>');
		}

		return name;
	});

	function formatResult(item, container, query) {

		var variables = item.standardVariables.sort(function(a, b) {
			return a.name.localeCompare(b.name);
		});

		return generateFormattedResult({
			propertyName: item.name,
			searchTerm: query.term,
			className: item.classesStr,
			variables: variables
		});
	}

	function formatSelection(item) {
		return '<strong>' + item.name + '</strong> (' + item.classesStr + ')';
	}

	/**
	 * Creates a new Property Select dropdown
	 *
	 * @constructor
	 * @param {string} container the selector referencing the container to which the dropdown should be attached
	 * @param {string} placeholder the label to use for a placeholder in the select
	 * @param {object[]} properties an array of properties to populate the dropdown
	 * @param {function} onSelect a function to call on dropdown select
	 */
	PropertySelect = function(container, placeholder, properties, onSelectFn) {

		this._$container = $(container);
		this._$container.append(generateDropdownMarkup());

		this._$select = this._initialiseSelect(placeholder, properties);

		this._$select.on('select2-selecting', onSelectFn);

		return this;
	};

	/**
	 * Sets the value of the dropdown to a specified property.
	 *
	 * @param {object} property the property object to set as the new selected value
	 */
	PropertySelect.prototype.setValue = function(property) {
		this._$select.select2('data', property);
	};

	/**
	 * Destroy the dropdown.
	 */
	PropertySelect.prototype.destroy = function() {
		this._$select.select2('destroy');
		this._$container.empty();
	};

	/*
	 * Initialise the select2 widget.
	 *
	 * @param {string} placeholder the placeholder to set inside the select
	 * @param {object[]} properties the properties to set as the contents of the dropdown
	 */
	PropertySelect.prototype._initialiseSelect = function(placeholder, properties) {

		return $(propertyDropdownSelector).select2({
			placeholder: placeholder,
			minimumResultsForSearch: 4,
			formatSelection: formatSelection,
			formatResult: formatResult,
			id: function(item) {
				return {
					id: item.propertyId
				};
			},
			query: function(options) {

				var searchTerm = options.term.toLocaleLowerCase(),
					result = {};

				result.results = $.grep(properties, function(prop) {

					var propContainsTerm,
						varNameContainsTerm,
						varAliasContainsTerm;

					// Include the object in the results if either the property name or class name contains the search term
					propContainsTerm = prop.name.toLocaleLowerCase().indexOf(searchTerm) > -1 ||
						prop.classesStr.toLocaleLowerCase().indexOf(searchTerm) > -1;

					if (propContainsTerm) {
						return true;
					}

					// Also include the object if any of the property's variables have the search term in their name
					$.each(prop.standardVariables, function(index, variable) {
						varNameContainsTerm = variable.name.toLocaleLowerCase().indexOf(searchTerm) > -1;
						varAliasContainsTerm = false;
						if (!varNameContainsTerm && variable.alias != null) {
							varAliasContainsTerm = variable.alias.toLocaleLowerCase().indexOf(searchTerm) > -1;
						}

						// Returning false will stop the loop - we want to stop when we've found the first variable
						// in this property that contains the search term
						return !varNameContainsTerm && !varAliasContainsTerm;
					});

					return varNameContainsTerm || varAliasContainsTerm;
				});

				options.callback(result);
			},
			dropdownCssClass: 'ps-results'
		});
	};

	return PropertySelect;

})(jQuery);
