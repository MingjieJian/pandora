      subroutine HIDRY
     $(QNAME,QELSM,QIONM,NCINM)
C
C     Rudolf Loeser, 1982 Jun 14
C---- Attempts to set QELSM equal to a valid element symbol,
C     on the basis of QNAME if necessary, and
C     then constructs the full ion name.
C     !DASH
      save
C     !DASH
      integer KIND, LOOK, NCINM, NOION
      character BLANKS*7, QELSM*8, QIONM*8, QNAME*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- ELEGANT     as of 2002 Mar 26
      integer     LNAM,LABR,NELE
      character   ENAMES*12,EABBRS*2
      dimension   LNAM(92),LABR(92),ENAMES(92),EABBRS(92)
      common      /ELEGNT1/ NELE,LNAM,LABR
      common      /ELEGNT2/ ENAMES,EABBRS
C     "ENAMES" is a list of element names, and "LNAM" specifies
C     the number of characters in each name.
C     "EABBRS" is the corresponding list of element symbols, and
C     "LABR" specifies the number of characters in each symbol.
C     .
C     !DASH
C     !EJECT
      external LOOKUC, MONIKA, HI, BYE
C
      data BLANKS /'       '/
C
      call HI ('HIDRY')
C     !BEG
      if(NOION.le.0) then
C
        if(QELSM(1:2).ne.'ZZ') then
          call LOOKUC (EABBRS, NELE, QELSM(1:2), KIND, LOOK)
          if(LOOK.ne.1) then
            QELSM = 'ZZ      '
          end if
        end if
C
        if(QELSM(1:2).eq.'ZZ') then
          call LOOKUC (ENAMES, NELE, QNAME,      KIND, LOOK)
          if(LOOK.eq.1) then
            QELSM = EABBRS(KIND)//BLANKS
          end if
        end if
C       (QELSM ends up in common /argus/, where MONIKA gets it.)
C
        call MONIKA   (1, QIONM, NCINM)
C
      end if
C     !END
      call BYE ('HIDRY')
C
      return
      end
