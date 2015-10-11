# This script will collect pitch data from annotated audio files (WAV/AIF + Praat TextGrid).
#
# Sound file name must consist of:
# conversationCode_speakerCode1[_speakerCode2][_speakerCode3].wav
# for the file extension, which must be .TextGrid.
# The interval tier to be used in the analysis of a particular speaker must be labeled 
# with the speaker's code that is used in the file names. 
# In case several speakers are present in the same audio file, the file name should 
# include all the corresponding speaker names/codes, separated with underscores.
# For instance:
# A03_F4_M2.wav and A03_F4_M2.TextGrid (for dialogue A03 with a female speaker F4 and 
# a male speaker M2)
# Or, if there are separate audio files available for each speaker in dialogue A03:
# A03_F4.wav and A03_F4.TextGrid for speaker F4, and
# A03_M2.wav and A03_M2.TextGrid for speaker M2.
#
# The analysis parameters for each speaker (or pair of WAV+TextGrid files) may optionally 
# be provided in the text file parms.txt.
# Result files:
#  - pitch_utterances_all.txt: values recorded from within annotated utterances only,
#         using the default parameters
#  - pitch_utterances_indi.txt: values recorded from within annotated utterances only 
#         using the speaker-specific parameters (in case they are defined in parms.txt)
#  - pitch_indi.txt: all values from those speakers who have their specific analysis 
# parameters defined in parms.txt
#  - pitch_all.txt: all values even outside min_pitch and max_pitch are included 
#
# .Pitch files:
# outputdir/soundname_raw.Pitch = all pitch values from raw audio, default max & min
# outputdir/[conversationID_]speaker.Pitch = raw audio, individual max & min parms
# The individual, short Pitch objects calculated from utterances are not saved.
#
# NOT IMPLEMENTED YET: 
# In case the speakers share the same audio file and overlap between 
# speakers is found, the overlapping portions should be recorded as 
# missing values (default NA).
#
# This script is distributed under the latest version of the GNU General Public License.
# 
# Mietta Lennes 9.9.2015

inputdir$ = "corpus/"
outputdir$ = "data/"
pitchdir$ = "pitch/"

# This is the time step between the consecutive pitch points whose values will be
# collected to the data file:
time_step_F0 = 0.02

# This is the maximum duration (seconds) of the audio file that will be included in the 
# analysis (0 = include the total duration):
max_duration = 720     
# 720 = a maximum of 12 minutes will be analyzed per file
# (1800 = a maximum of 30 minutes will be analyzed per file)

# The default pitch analysis parameters to be applied when individual parms are not 
# available:
default_min = 50
default_max = 600

# The amount of time that should be included in the pitch analysis window around
# each annotated utterance:
utterance_margin = 0.3
# (0.3 seconds = 300 milliseconds)

# The string to use for undefined or missing values:
empty_field$ = "NA"

writeInfoLine: "Collect pitch samples from files in 'inputdir$'"

# Look for audio files in the input directory:
Create Strings as file list: "wavList","'inputdir$'*.wav"
Create Strings as file list: "aifList","'inputdir$'*.aif"
numberOfAif = Get number of strings
if numberOfAif > 0
	selectObject: "Strings wavList"
	plusObject: "Strings aifList"
	Append
	Rename: "soundList"
	selectObject: "Strings wavList"
	plusObject: "Strings aifList"
	Remove
else
	selectObject: "Strings wavList"
	Rename: "soundList"
	selectObject: "Strings aifList"
	Remove
endif

# Read in the individual pitch analysis parameters (in case such a file exists):
Read Table from tab-separated file: "'inputdir$'parms.txt"
Rename: "parameters"

# Define the paths and names of the output files
#
# The data obtained with the general min and max parameters:
outputfile_all$ = outputdir$ + "pitch_all.txt"
# The data obtained with individually defined min and max parameters:
outputfile_indi$ = outputdir$ + "pitch_indi.txt"
# The data obtained with the default min and max parameters from within annotated utterances:
outputfile_utt_all$ = outputdir$ + "pitch_utterances_all.txt"
# The data obtained with individually defined min and max parameters from within annotated utterances:
outputfile_utt_indi$ = outputdir$ + "pitch_utterances_indi.txt"
# Old output files will be deleted, in case they exist!!!
deleteFile: outputfile_utt_all$
deleteFile: outputfile_utt_indi$
deleteFile: outputfile_indi$
deleteFile: outputfile_all$
#
# Add a title row to the output files:
titleline$ = "File	Conversation	Speaker	Gender	PitchHz	PitchST	AbsTime'newline$'"
appendFile: outputfile_indi$, titleline$
appendFile: outputfile_all$, titleline$
titleline$ = "File	Conversation	Speaker	Gender	UtteranceNr	PitchHz	PitchST	AbsTime	UtteranceInt'newline$'"
appendFile: outputfile_utt_indi$, titleline$

selectObject: "Strings soundList"
Sort
numberOfFiles = Get number of strings

for file to numberOfFiles
	select Strings soundList
	sound$ = Get string: file
	Read from file: "'inputdir$''sound$'"
	appendInfoLine: "--- Sound file 'sound$':"
	soundname$ = selected$("Sound")
	if max_duration > 0
		total_duration = Get total duration
		if total_duration > max_duration
			Extract part: 0, max_duration, "rectangular", 1.0, 1
			Rename: "extracted"
			selectObject: "Sound 'soundname$'"
			Remove
			selectObject: "Sound extracted"
			Rename: soundname$
			writeInfoLine: "NB: Only the first 'max_duration' seconds of file 'soundname$' will be analyzed."
		endif
	endif

	# Parse the sound file name and extract speaker names from it (in case 
	# there are several speakers in the same file):
	speakers = 0
	conversation_code$ = empty_field$
	speakers$ = soundname$
	if index(speakers$, "_") > 0
		conversation_code$ = left$(speakers$, (index(speakers$, "_") - 1))
		speakers$ = right$(speakers$, length(speakers$) - (index(speakers$, "_")))
	endif
	while index(speakers$, "_") > 0
		speakers = speakers + 1
		speaker_'speakers'$ = left$(speakers$, (index(speakers$, "_") - 1))
		speakers$ = right$(speakers$, length(speakers$) - (index(speakers$, "_")))
	endwhile
	speakers = speakers + 1
	speaker_'speakers'$ = speakers$

	call AnalyzeRawAudioFile
	
	textgrid$ = inputdir$ + soundname$ + ".TextGrid"
	if fileReadable (textgrid$)
		# Open files to object list
		Read from file: textgrid$
		gridname$ = selected$ ("TextGrid")
		for speaker to speakers
			speaker$ = speaker_'speaker'$
			# Proceed to analyses:
			utterances = 0
			appendInfoLine: "Analyzing utterances for speaker: 'speaker$' ('textgrid$')"
			call AnalyzeAnnotatedFile
		endfor
		# remove TextGrid object from object list
		selectObject: "TextGrid 'gridname$'"
		Remove
	else
		appendInfoLine: "The file 'textgrid$' does not exist." 
		appendInfoLine: "Pitch data was calculated from raw audio only."		
	endif

	selectObject: "Sound 'soundname$'"
	Remove
	pause Continue?
	
endfor



#------
procedure AnalyzeAnnotatedFile

selectObject: "TextGrid 'gridname$'"
call GetTier 'speaker$' utterance_tier
if utterance_tier = 0
	call GetTier utterance utterance_tier
endif

if utterance_tier > 0

	gender$ = empty_field$
	if left$ (speaker$, 1) = "F"
		gender$ = "female"
	elsif left$ (speaker$, 1) = "M"
		gender$ = "male"
	endif

	#------- Begin analysis

	call GetPitchParameters
	selectObject: "TextGrid 'gridname$'"
	numberOfUtterances = Get number of intervals... utterance_tier

	for utterance to numberOfUtterances
		selectObject: "TextGrid 'gridname$'"
		utterance$ = Get label of interval... utterance_tier utterance
		if utterance$ <> "" and left$ (utterance$, 1) <> "." and utterance$ <> "xxx"
			utterances = utterances + 1
			uttstart = Get starting point... utterance_tier utterance
			uttend = Get end point... utterance_tier utterance
			winstart = uttstart - utterance_margin
			if winstart < 0
				winstart = 0
			endif
			winend = uttend + utterance_margin
			if winend > total_duration
				winend = total_duration
			endif
			if max_duration > 0 and winend > max_duration
				winend = max_duration
			endif

			# If the extracted time window is at least 100 ms long, analyse it:
			if winend - winstart > 0.1
				selectObject: "Sound 'soundname$'"
				Extract part: winstart, winend, "rectangular", 1.0, 1
				Rename: "extracted"
				# First, do the pitch analysis using the default parameters:
				To Pitch... time_step_F0 default_min default_max
				numberOfPitchPoints = Get number of frames
				pitchpoint = Get frame number from time... uttstart
				if pitchpoint = 0
					pitchpoint = 1
				endif
				pitchtime = Get time from frame number... pitchpoint
				if pitchtime < uttstart and pitchpoint < numberOfPitchPoints
					pitchpoint = pitchpoint + 1
					pitchtime = Get time from frame number... pitchpoint
				endif
				while pitchtime < uttend
					pitchvalueHz$ = "'empty_field$'"
					pitchvalueST$ = "'empty_field$'"
				
					pitchvalueHz = Get value in frame... pitchpoint Hertz
					pitchvalueST = Get value in frame... pitchpoint semitones re 100 Hz
					if pitchvalueHz <> undefined
						pitchvalueHz$ = "'pitchvalueHz'"
						pitchvalueST$ = "'pitchvalueST'"
					endif

					pitchtimeUtt = (pitchtime - uttstart) / (uttend - uttstart)
					pitchtimeUtt$ = "'pitchtimeUtt'"
							
					resultline$ = "'gridname$'	'conversation_code$'	'speaker$'	'gender$'	'utterances'	'pitchvalueHz$'	'pitchvalueST$'	'pitchtime'	'utterance'	'pitchtimeUtt$''newline$'"

					fileappend 'outputfile_utt_all$' 'resultline$'
				
					pitchpoint = pitchpoint + 1
					pitchtime = Get time from frame number... pitchpoint
				endwhile
				# In case speaker-specific parameters are available, calculate Pitch for 
				# this utterance using those:
				if min_pitch <> default_min or max_pitch <> default_min
					selectObject: "Sound extracted"
					To Pitch... time_step_F0 min_pitch max_pitch
					numberOfPitchPoints = Get number of frames
					pitchpoint = Get frame number from time... uttstart
					if pitchpoint = 0
						pitchpoint = 1
					endif
					pitchtime = Get time from frame number... pitchpoint
					if pitchtime < uttstart and pitchpoint < numberOfPitchPoints
						pitchpoint = pitchpoint + 1
						pitchtime = Get time from frame number... pitchpoint
					endif
					while pitchtime < uttend
						pitchvalueHz$ = "'empty_field$'"
						pitchvalueST$ = "'empty_field$'"
				
						pitchvalueHz = Get value in frame... pitchpoint Hertz
						pitchvalueST = Get value in frame... pitchpoint semitones re 100 Hz
						if pitchvalueHz <> undefined
							pitchvalueHz$ = "'pitchvalueHz'"
							pitchvalueST$ = "'pitchvalueST'"
						endif
				
						resultline$ = "'gridname$'	'conversation_code$'	'speaker$'	'gender$'	'utterances'	'pitchvalueHz$'	'pitchvalueST$'	'pitchtime'	'utterance''newline$'"

						fileappend 'outputfile_utt_indi$' 'resultline$'
				
						pitchpoint = pitchpoint + 1
						pitchtime = Get time from frame number... pitchpoint
					endwhile
					Remove
				endif
				selectObject: "Sound extracted"
				plusObject: "Pitch extracted"
				Remove
			endif
		endif
	endfor
	writeInfoLine: "'utterances' utterances were analyzed in 'gridname$'."

else
	appendInfoLine: "No utterance tier found in 'gridname$' for speaker 'speaker$'. Skipping..."
endif

endproc

#------
procedure AnalyzeRawAudioFile

	pitchfile_all$ = pitchdir$ + soundname$ + "_raw.Pitch"

	if fileReadable(pitchfile_all$) = 0
		min_pitch = default_min
		max_pitch = default_max
		# Create the Pitch object:
		selectObject: "Sound 'soundname$'"
		appendInfoLine: "Calculating pitch with default parameters for 'sound$'..."
		To Pitch... time_step_F0 min_pitch max_pitch
		Rename: "'soundname$'_raw"
		Write to short text file: pitchfile_all$
	else
		Read from file: pitchfile_all$
	endif

	numberOfPitchPoints_all = Get number of frames

	for pitchpoint to numberOfPitchPoints_all
			pitchtime = Get time from frame number: pitchpoint
			pitchvalueHz$ = "'empty_field$'"
			pitchvalueST$ = "'empty_field$'"
			
			pitchvalueHz = Get value in frame: pitchpoint, "Hertz"
			pitchvalueST = Get value in frame: pitchpoint, "semitones re 100 Hz"
			# Only defined pitch values will be recorded for the raw audio, in order to 
			# save space.
			if pitchvalueHz <> undefined
				pitchvalueHz$ = "'pitchvalueHz'"
				pitchvalueST$ = "'pitchvalueST'"			
				speaker$ = empty_field$
				gender$ = empty_field$
				if speakers = 1
					speaker$ = speaker_1$
					if left$ (speaker$, 1) = "F"
						gender$ = "female"
					elsif left$ (speaker$, 1) = "M"
						gender$ = "male"
					endif
				endif
				resultline$ = "'soundname$'	'conversation_code$'	'speaker$'	'gender$'	'pitchvalueHz$'	'pitchvalueST$'	'pitchtime''newline$'"
				# Record this pitch value to the text file:
				fileappend 'outputfile_all$' 'resultline$'
			endif
	endfor
	
	# In case the audio file represents only one speaker and if there are individual 
	# parameters available, analyze the complete file with those parameters:
	if speakers = 1
		speaker$ = speaker_1$
		if conversation_code$ <> empty_field$
			conv$ = conversation_code$ + "_"
		else
			conv$ = ""
		endif
		call GetPitchParameters
		if min_pitch <> default_min or max_pitch <> default_min
			pitchfile$ = pitchdir$ + conv$ + speaker$ + ".Pitch"
			selectObject: "Sound 'soundname$'"
			To Pitch... time_step_F0 min_pitch max_pitch
			Rename: "'soundname$'"
			Write to short text file: pitchfile$
		endif
		numberOfPitchPoints = Get number of frames
		appendInfoLine: "Analyzing pitch from raw audio for speaker 'speaker$'..."
		for pitchpoint to numberOfPitchPoints_all
			pitchtime = Get time from frame number: pitchpoint
			pitchvalueHz$ = "'empty_field$'"
			pitchvalueST$ = "'empty_field$'"
			
			pitchvalueHz = Get value in frame: pitchpoint, "Hertz"
			pitchvalueST = Get value in frame: pitchpoint, "semitones re 100 Hz"
			# Only defined pitch values will be recorded for the raw audio, in order to 
			# save space.
			if pitchvalueHz <> undefined
				pitchvalueHz$ = "'pitchvalueHz'"
				pitchvalueST$ = "'pitchvalueST'"
				gender$ = empty_field$
				if left$ (speaker$, 1) = "F"
					gender$ = "female"
				elsif left$ (speaker$, 1) = "M"
					gender$ = "male"
				endif
				resultline$ = "'soundname$'	'conversation_code$'	'speaker$'	'gender$'	'pitchvalueHz$'	'pitchvalueST$'	'pitchtime''newline$'"
				# Record this pitch value to the text file:
				fileappend 'outputfile_indi$' 'resultline$'
			endif
		endfor
	endif

endproc



#-------------
procedure GetTier name$ variable$
	numberOfTiers = Get number of tiers
	itier = 1
	repeat
		tier$ = Get tier name... itier
		itier = itier + 1
	until tier$ = name$ or itier > numberOfTiers
	if tier$ <> name$
		'variable$' = 0
	else
		'variable$' = itier - 1
	endif
	
endproc

#--------
procedure GetPitchParameters

min_pitch = 50
max_pitch = 500

selectObject: "Table parameters"
row = Search column: "speaker", speaker$
if row > 0
	min_pitch = Get value: row, "min"
	max_pitch = Get value: row, "max"
endif

appendInfoLine: "'speaker$' min:'min_pitch' max:'max_pitch'"

endproc

#------------------

# The following procedure is not applied yet in the main script (for later use):

procedure FulfilsCriterion sel_tier sel_interval crittier crittext$

selectObject: "TextGrid 'gridname$'"
in_interval = 0

if crittier > 0
	tempstart1 = Get starting point... sel_tier sel_interval
	tempend1 = Get end point... sel_tier sel_interval
	midtime1 = (tempstart1 + tempend1) / 2

	tempcriterion = Get interval at time... crittier midtime1
	tempstart2 = Get starting point... crittier tempcriterion
	tempend2 = Get end point... crittier tempcriterion

	temp_label2$ = Get label of interval... crittier tempcriterion

	# if criterion text is empty, any interval label other than "" will be accepted
	if crittext$ = "" and temp_label2$ <> ""
		crittext$ = temp_label2$
	endif

	if tempstart2 <= tempstart1 and tempend2 >= tempend1
		in_interval = tempcriterion
	endif

	if temp_label2$ = crittext$ and tempstart2 <= tempstart1 and tempend2 >= tempend1
		fulfils = 1
	else
		fulfils = 0
	endif
else 
	fulfils = 1
endif

endproc


