

! Array and Character lengths
MODULE ARRAYLEN

INTEGER, PARAMETER :: FSTR=150 ! maximum length of the parsed line
INTEGER, PARAMETER :: HSTR=75  ! maximum length of the key/value
INTEGER, PARAMETER :: KLST=10  ! number of keys in the key list
INTEGER, PARAMETER :: FLEN=20  ! maximum filename+path length

END MODULE

! Init Variables
MODULE INITDATA

USE ARRAYLEN, ONLY : HSTR
INTEGER LEASTNG,BNRTHNG,POINTEW,POINTNS,RES
LOGICAL OSFREQ
CHARACTER(LEN=HSTR) PROJEC,INIFILE,VERS

END MODULE

!--------------MAIN PROGRAM START------------------------------
! this program is designed to read from a user-defined file and
! parse the contents following the keys defined in keyfile.txt
! into the variables in MODULE INITDATA using the IF tree in
! READINIT. Expanding these three components would allow easy
! extension to include new keys. RAWVAL contains a string of
! the value to the right of = in the input file and KEY1 contains
! the key to match it (left of =).
! For a character value, RAWVAL does not need modification
! For an interger value, RAWVAL can be converted using MAKEINT
! For a boolean value, RAWVAL can be converted using MAKEBOOL

PROGRAM PARSER
USE INITDATA
USE ARRAYLEN, ONLY : FLEN
IMPLICIT NONE
CHARACTER(LEN=FLEN) FILENAME
CHARACTER RESPONSE
LOGICAL IEXIST


! Inquire for the file to be parsed
PRINT *, "Please enter the name and location of the file to be&
  & parsed"
  READ(*,*) FILENAME

!FILENAME="InitInt.txt"
! This is the main subroutine, selfcontained for ease of implementation
! in other programs.

CALL READINIT(FILENAME)

! Output contents to screen.

WRITE(*,*) "-------------------------------------------------------"
WRITE(*,'(A20,": ",A)') "Version Number ",VERS
WRITE(*,'(A20,": ",A)') "Projection ",PROJEC
WRITE(*,'(A20,": ",A)') "IntFile ",TRIM(INIFILE)
WRITE(*,'(A20,": ",I10)') "LEasting ",LEASTNG
WRITE(*,'(A20,": ",I10)') "BNorthing ",BNRTHNG
WRITE(*,'(A20,": ",I10)') "NumPoints (EW) ",POINTEW
WRITE(*,'(A20,": ",I10)') "NumPoints (NS) ",POINTNS
WRITE(*,'(A20,": ",I10)') "Resolution ",RES
WRITE(*,'(A20,": ",L10)') "Only Same Frequency ",OSFREQ
WRITE(*,*) "-------------------------------------------------------"
PRINT *, "PRESS ANY KEY AND ENTER TO CLOSE"
READ(*,*) RESPONSE

END

!--------------READINIT SUBROUTINE------------------------------

SUBROUTINE READINIT(FILENAME)

! this subroutine takes in a FILENAME, opens it, parses its data
! according to keylist.txt and saves the values (RAWVAL) into the
! correct variables in INITDATA using the IF tree.

USE INITDATA
USE ARRAYLEN
CHARACTER(LEN=FLEN) FILENAME,KEYLIST(KLST),KLFNAME
CHARACTER(LEN=FSTR) KEY1
CHARACTER(LEN=HSTR) RAWVAL
INTEGER :: EOF=0
INTEGER MAKEINT
LOGICAL MAKEBOOL,IEXIST

! open the keylist file and load in the keys to search for

KLFNAME="keylist.txt"
CALL SOPEN(KLFNAME,10)

DO I=1,KLST
  READ (10,*,IOSTAT=EOF) KEYLIST(I)
  IF (EOF.LT.0) EXIT ! END OF FILE, ALL DATA READ
ENDDO
CLOSE(10)

CALL SOPEN(FILENAME,20)

 DO
  READ(20,'(A)',IOSTAT=EOF) KEY1 ! read a line to parse
  IF (EOF.LT.0) EXIT ! end of file, escape condition

! Take the read line, convert to lowercase and seperact it into
! key and value components.

  CALL STRINGPROCESS(KEY1,RAWVAL)

! This block uses the processed KEY1 (keyword) to convert and
! assign RAWVAL to its correct variable. MAKEINT converts to an
! integer, MAKEBOOL converts to a boolean.

  IF (KEY1.EQ.KEYLIST(1)) THEN
    VERS=RAWVAL 
  ELSEIF (KEY1.EQ.KEYLIST(2)) THEN
    PROJEC=RAWVAL
  ELSEIF (KEY1.EQ.KEYLIST(3)) THEN
    INIFILE=RAWVAL
  ELSEIF (KEY1.EQ.KEYLIST(4)) THEN
    LEASTNG=MAKEINT(RAWVAL) 
  ELSEIF (KEY1.EQ.KEYLIST(5)) THEN
    BNRTHNG=MAKEINT(RAWVAL)
  ELSEIF (KEY1.EQ.KEYLIST(6)) THEN
    POINTEW=MAKEINT(RAWVAL)
  ELSEIF (KEY1.EQ.KEYLIST(7)) THEN
    POINTNS=MAKEINT(RAWVAL)
  ELSEIF (KEY1.EQ.KEYLIST(8)) THEN
    RES=MAKEINT(RAWVAL)
  ELSEIF (KEY1.EQ.KEYLIST(9)) THEN
    OSFREQ=MAKEBOOL(RAWVAL)
  ENDIF
  
  ENDDO
CLOSE(20)

RETURN
END

!--------------MAKEINT FUNCTION---------------------------------

INTEGER FUNCTION MAKEINT(RAWVAL)

! This function converts a string to an integer by 
! manipulating the ASCII table values for 0-9.
USE ARRAYLEN, ONLY : HSTR
CHARACTER(LEN=HSTR) RAWVAL
INTEGER I

! 48=0 TO 57=9 CONVERSION REQUIRES -48
MAKEINT=0
DO I=1,LEN(TRIM(RAWVAL))
  IF ((RAWVAL(I:I).LT."0").OR.(RAWVAL(I:I).GT."9")) THEN
    PRINT *, "ERROR: ",RAWVAL(I:I)," IS NOT A NUMBER"
    STOP
  ENDIF
  MAKEINT=MAKEINT*10+IACHAR(RAWVAL(I:I))-48
ENDDO

RETURN
END

!--------------MAKEBOOL FUNCTION-------------------------------

LOGICAL FUNCTION MAKEBOOL(RAWVAL)

! This function takes in a character and tries to convert
! it to a boolean, if the first letter is t or T it becomes?
! true, f or F return false, and anything else throws an error.
USE ARRAYLEN, ONLY : HSTR
CHARACTER(LEN=HSTR) RAWVAL

CALL LOWERCASE(RAWVAL,LEN(TRIM(RAWVAL)))

IF ((RAWVAL(1:2).EQ."f ").OR.(RAWVAL(1:5).EQ."false")) THEN
  MAKEBOOL=.FALSE.
ELSEIF ((RAWVAL(1:2).EQ."t ").OR.(RAWVAL(1:4).EQ."true")) THEN
  MAKEBOOL=.TRUE.
ELSE
  PRINT *, "ERROR: ",RAWVAL," IS NOT A VALID BOOLIAN"
  STOP
ENDIF

RETURN
END

!--------------STRINGPROCESS SUBROUTINE-------------------------

SUBROUTINE STRINGPROCESS(STRING,RAWVAL)

! This subroutine is designed to take in a string (STRING) of 
! format keyword(s) = value and seperate it out into
! lowercase keywords(s) and value (RAWVAL).
USE ARRAYLEN, ONLY : FSTR,HSTR
INTEGER I,N,EQPOS
CHARACTER(LEN=FSTR) STRING
CHARACTER(LEN=HSTR) RAWVAL


! First the position of the "=" deliminator needs to be found
N=1
DO I=1,LEN(STRING)
  IF (STRING(I:I).EQ."=") THEN
    EXIT
  ENDIF
  N=N+1
ENDDO

! Throw an error if there is no = deliminator
IF (N.EQ.LEN(STRING)) THEN
  PRINT *, "ERROR: NO '=' FOUND ON LINE"
  STOP
ENDIF

EQPOS=N ! position of the = sign

! call the subroutine to convert the key section to lowercase
CALL LOWERCASE(STRING,N-1)

! find the first non space character for the value
DO I=N+1,LEN(STRING)
 IF (STRING(I:I).EQ." ") THEN
   N=N+1
 ELSE
   EXIT
 ENDIF
ENDDO

! store the value for this key in RAWVAL as a string
RAWVAL=STRING(N+1:LEN(STRING))

! replaces anything after the keywords with " "
DO I=EQPOS,LEN(STRING)
  STRING(I:I)=" "
ENDDO

RETURN
END

SUBROUTINE LOWERCASE(STRING,N)

! The standard ASCII table defines character values as:
! A-Z(65-90) a-z(97-122) so +32 changes the case.
! This section converts to STRING to lowercase up to N

USE ARRAYLEN, ONLY : FSTR
INTEGER N,I
CHARACTER(LEN=FSTR) STRING

DO I=1,N
  IF ((STRING(I:I).GE."A").AND.(STRING(I:I).LE."Z")) THEN
    STRING(I:I)=ACHAR(IACHAR(STRING(I:I))+32) ! change to lowercase
  ENDIF
ENDDO

RETURN
END

SUBROUTINE SOPEN(FILENAME,U)
! this subroutine opens a FILENAME but first checks it exists.

USE ARRAYLEN, ONLY : FLEN
INTEGER U
LOGICAL IEXIST
CHARACTER(LEN=FLEN) FILENAME

INQUIRE(FILE=FILENAME,EXIST=IEXIST)
IF (IEXIST.EQV..FALSE.) THEN
  PRINT *, FILENAME," DOES NOT EXIST"
  STOP
ENDIF
OPEN(U,FILE=FILENAME)

RETURN
END

