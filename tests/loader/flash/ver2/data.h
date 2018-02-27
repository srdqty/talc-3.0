/* version of datacache.h, but without support for caching.  In other
   words, it just keeps track of the data associated with the request
   but doesn't worry about storing it long term ... */

extern ?struct DataEntry {
  string ce_filename;		
  /* CacheEntry ce_nextInHash; */
  /* struct CacheEntry **ce_prevInHash; */
  /* CacheEntry ce_nextInLRU; *//* not impl */
  int ce_size;
  /* int ce_numRefs; */
  /* int ce_isInvalid; */
  int ce_file_fd;
  int ce_numChunks;		/* <= size(ce_datachunks) */
  /* int ce_numChunksInMem; */
  string ce_dataChunks[];
  /* int ce_lastValidation; */
  string ce_encodings;		/* from figure_mime call */
  string ce_type;		/* from figure_mime call */
  int ce_modTime;
  string ce_respHeader;		/* memory allocated for mime */
  int ce_respHeaderLen;
  int ce_respHeaderTime;	/* date field in resp header */
  /* string ce_200Resp; */	/* cached response for log */
  /* int ce_200RespLen;	*/	/* length of 200 response */
}

extern DataEntry GetEmptyDataEntry();
extern void CompleteDataEntry(DataEntry ce);
extern bool CalcRespHeader(DataEntry ce, int status, string title);
extern *(string,int,int) GetDataToSend(DataEntry ent, int position, 
				       int desiredSize, int maxSize);
extern void printDataEntry(DataEntry ce);
