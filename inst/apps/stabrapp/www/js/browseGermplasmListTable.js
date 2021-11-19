/*exported startIndex,germplasmStartingEntry,importLocationUrl,importIframeOpened,overwriteChecksList*/
/*exported confirmReplaceList,resetGermplasmList,removeCheckFromList,openListTree,viewGermplasmLitDetails */
/*exported additionalLazyLoadUrl, chooseList*/
/*globals angular*/
var listId = 0,
	startIndex = 0,
	lastDraggedPrimaryList = 0,
	lastDraggedChecksList = 0;

//to be use as reference to the data table object
var germplasmDataTable = null;
