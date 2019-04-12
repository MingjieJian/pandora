      subroutine PINA
     $(LU,INAME,INDEX)
C
C     Rudolf Loeser, 2004 Oct 21
C---- Sets up ion name and index for calculation of CP,
C     the photoionization cross section.
C     Returns INDEX = 0 if this calculation does not apply.
C
C---- If input parameter LU > 0, then does nothing but write
C     ion list to unit LU.
C     !DASH
      save
C     !DASH
      integer I, INDEX, K, LOOK, LU, LUEO, NT, jummy
      character INAME*3, QIONM*8, TAB*3
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LOOKUC, MONIKA, MESHED, ABORT, HI, BYE
C
      parameter (NT=45)
      dimension TAB(NT)
C
      data TAB /
     $ 'H1 ', 'HE1', 'HE2', 'B1 ', 'C1 ', 'C2 ', 'C3 ', 'C4 ', 'N1 ',
     $ 'N2 ', 'O1 ', 'O2 ', 'O3 ', 'O4 ', 'O5 ', 'O6 ', 'NA1', 'MG1',
     $ 'MG2', 'AL1', 'AL2', 'SI1', 'SI2', 'SI3', 'SI4', 'S1 ', 'CA1',
     $ 'CA2', 'FE1', 'C5 ', 'N3 ', 'N4 ', 'NE1', 'NE2', 'NE3', 'NE4',
     $ 'NE5', 'NE6', 'NE7', 'NE8', 'NE9', 'NA2', 'S2 ', 'S3 ', 'S4 '/
C
      call HI ('PINA')
C     !BEG
      if(LU.gt.0) then
        write (LU,100)
  100   format(' ',10X,'List of ions with CP(1) and/or RRCP(1) data:')
        write (LU,101) (TAB(I),I=2,NT)
  101   format(' ',10X,20A5)
        goto 104
      end if
C     !EJECT
      INDEX = 0
C
      call MONIKA   (2, QIONM, jummy)
      INAME = QIONM(:3)
      call LOOKUC   (TAB, NT, INAME, K, LOOK)
      if(LOOK.eq.1) then
        INDEX = K
      end if
C
      if(INDEX.eq.0) then
        call MESHED ('PINA', 1)
        write (LUEO,103) INAME
  103   format(' ','INAME = ',A,'; PINA has no data for this --- '/
     $         ' ','thus input values of CP and/or RRCP for level 1 ',
     $             'must be provided.')
        call ABORT
      end if
C
  104 continue
C     !END
      call BYE ('PINA')
C
      return
      end
