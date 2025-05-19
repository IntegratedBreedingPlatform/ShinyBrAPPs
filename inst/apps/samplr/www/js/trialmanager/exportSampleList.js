/*globals $,showErrorMessage, console  */
/*exported checkBeforeExport, showSampleListExportOptions */
var ExportSampleList = {};
(function () {
	'use strict';
	var exportType,sampleListId = 0, sampleListName = "", submitExportUrl = '/Fieldbook/ExportManager/';


	ExportSampleList.checkBeforeExport = function () {
		'use strict';
		exportType = $('#exportSampleListModalBody #exportType').val();

		if (exportType === '2') {
			showErrorMessage('', 'This option is not available');
			return false;
		}
		ExportSampleList.doExport(exportType);
	};

	//TODO THIS METHOD IS GONNA BY THE PRINCIPAL FOR USE THE EXPORT UI.
	ExportSampleList.showSampleListExportOptions = function (listId, listName) {
		'use strict';
		sampleListId = listId;
		sampleListName = listName;
		$('#exportSampleListModal #heading-modal').html(exportHeader + " sample list");
		$('#exportSampleListModal').modal({backdrop: 'static', keyboard: true});

	};
	ExportSampleList.showExportResponse = function (responseText) {
		'use strict';
		var resp = $.parseJSON(responseText);

		if (resp.isSuccess) {
			$('#exportSampleListDownloadForm #outputFilename').val(resp.outputFilename);
			$('#exportSampleListDownloadForm #filename').val(resp.filename);
			$('#exportSampleListDownloadForm #contentType').val(resp.contentType);
			$('#exportSampleListDownloadForm').submit();
		} else {
			showErrorMessage('', resp.errorMessage);
		}

		$('#exportSampleListModal').modal('hide');

	};

	// TODO THIS METHOD IS USED TO export CSV file without UI Export Sample List.
	ExportSampleList.exportFile = function (listId, listName) {
		'use strict';
		sampleListId = listId;
		sampleListName = listName;
		ExportSampleList.doExport(6);// Export CSV

	};

	ExportSampleList.doExport = function (exportType) {
		'use strict';
		var newAction,
			studyName = $('#studyName').val(),
			visibleColumns;

		newAction = submitExportUrl + 'exportSampleList/' + exportType;

		visibleColumns = ExportSampleList.getTableVisibleColumns();
		$.ajax(newAction, {
			headers: {
				'Accept': 'application/json',
				'Content-Type': 'application/json'
			},
			data: JSON.stringify({
				'visibleColumns': visibleColumns,
				'listname': sampleListName,
				'listId': sampleListId,
				'studyname': studyName
			}),
			type: 'POST',
			dataType: 'text',
			success: function(data) {
				ExportSampleList.showExportResponse(data);
			}
		});
	};

	ExportSampleList.getTableVisibleColumns = function () {
		'use strict';
		return $('#sample-list-' + sampleListId + '_wrapper div.dataTables_scrollHead thead > tr > th').map(function() {
			return $(this).text();
		}).get().join(",")
	}
})();
