      subroutine ZIKIRTU
     $(KOMPO,NR,KWC,INWVC,KAPSMP,NP,NT,NV)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Reads a KAPSMP array, for AMBRO.
C     !DASH
      save
C     !DASH
      integer INWVC, KAPSMP, KOMPO, KWC, LUEO, NP, NR, NT, NV
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      dimension KAPSMP(NP,NT,NV)
C
      call HI ('ZIKIRTU')
C     !BEG
  100 continue
        read(KOMPO,101,end=102) KAPSMP
  101   format(2X,13I6)
        NR = NR+1
        if(NR.ne.INWVC) goto 100
      goto 104
C
  102 continue
        call MESHED ('ZIKIRTU',1)
        write (LUEO,103) NR,KWC,INWVC,NP,NT,NV
  103   format(' ','Trouble reading Composite Line Opacity data: ',
     $             'unexpected end-of-file.'//
     $         ' ','NR',I7,5X,'KWC',I7,5X,'INWVC',I7,10X,'NP',I5,5X,
     $             'NT',I5,5X,'NV',I5)
        call ABORT
C
  104 continue
C     !END
      call BYE ('ZIKIRTU')
C
      return
      end
