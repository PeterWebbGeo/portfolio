! Init Variables

MODULE INITDATA

INTEGER LEASTNG,BNRTHNG,POINTEW,POINTNS,RES
LOGICAL OSFREQ
CHARACTER*75 PROJEC,INIFILE
CHARACTER*8 VERS

END MODULE

!--------------MAIN PROGRAM START------------------------------

PROGRAM PARSER
USE INITDATA
IMPLICIT NONE
CHARACTER*20 FILENAME
CHARACTER RESPONSE
LOGICAL IEXIST


! Inquire for the file to be parsed and check it is a valid file
DO
  PRINT *, "Please enter the name and location of the file to be&
  & parsed"
  READ(*,*) FILENAME

! FILENAME="InitInt.txt"

  INQUIRE(FILE=FILENAME,EXIST=IEXIST)
  IF (IEXIST.EQV..FALSE.) THEN
    PRINT *, "INVALID FILENAME, RE-ENTER? (Y/N)"
    READ(*,*) RESPONSE
    IF ((RESPONSE.EQ."N").OR.(RESPONSE.EQ."n")) STOP
  ELSE
    EXIT
  ENDIF
ENDDO

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

! This subroutine is designed to read from the recived file
! and parse the lines using the predefined keys in keyfile.txt
! to add more keys, add them to keyfile.txt, the variables to
! INITDATA module and extend the ELSEIF tree accordingly.

USE INITDATA

INTEGER, PARAMETER :: KLEND=10
CHARACTER*20 FILENAME,KEYLIST(KLEND)
CHARACTER*150 KEY1
CHARACTER*75 RAWVAL
INTEGER EOF,MAKEINT
LOGICAL MAKEBOOL

EOF=0
OPEN(10,FILE="keylist.txt")
DO I=1,10
  READ (10,*,IOSTAT=EOF) KEYLIST(I)
  IF (EOF.LT.0) EXIT ! END OF FILE, ALL DATA READ
ENDDO
CLOSE(10)

OPEN(20,FILE=FILENAME)

 DO
  READ(20,'(A)',IOSTAT=EOF) KEY1
  IF (EOF.LT.0) EXIT ! end of file, escape condition

! Take the read line, convert to lowercase and seperact it into
! key and value components.

  CALL STRINGPROCESS(KEY1,RAWVAL)

! This block uses the processed KEY1 (keyword) to convert and
! assign RAWVAL to its correct variable. MAKEINT converts to an
! integer, MAKEBOOL converts to a boolian.

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

CHARACTER*20 RAWVAL
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
! it to a boolian, if the first letter is t or T it becomes
! true, f or F return false, and anything else throws an error.

CHARACTER RAWVAL

IF ((RAWVAL(1:1).EQ."f").OR.(RAWVAL(1:1).EQ."F")) THEN
  MAKEBOOL=.FALSE.
ELSEIF ((RAWVAL(1:1).EQ."t").OR.(RAWVAL(1:1).EQ."T")) THEN
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

INTEGER I,N,EQPOS
CHARACTER*150 STRING
CHARACTER*75 RAWVAL

! The standard ASCII table defines character values as:
! A-Z(65-90) a-z(97-122) so +32 changes the case.
! This section converts the keywords to lowercase for matching.

N=1
DO I=1,LEN(STRING)
  N=N+1
  IF (STRING(I:I).EQ."=") THEN ! end conversion at occourance of "="
    EXIT
  ELSEIF ((STRING(I:I).GE."A").AND.(STRING(I:I).LE."Z")) THEN
    STRING(I:I)=ACHAR(IACHAR(STRING(I:I))+32) ! change to lowercase
  ENDIF
ENDDO

IF (N-1.EQ.LEN(STRING)) THEN
  PRINT *, "ERROR: NO '=' FOUND ON LINE"
  STOP
ENDIF

EQPOS=N ! position of the = sign

! remove any spaces before the value
DO I=N,LEN(STRING)
 IF (STRING(I:I).EQ." ") THEN
   N=N+1
 ELSE
   EXIT
 ENDIF
ENDDO
! store the value for this key in RAWVAL as a string
RAWVAL=STRING(N:LEN(STRING))

! replaces anything after the keywords with " "
DO I=EQPOS-1,LEN(STRING)
  STRING(I:I)=" "
ENDDO

RETURN
END
