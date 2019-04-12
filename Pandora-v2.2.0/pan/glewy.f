      subroutine GLEWY
     $(STAB,N)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Prints standard rates integrateions wavelengths table.
C     !DASH
      save
C     !DASH
      real*8 STAB
      integer I, J, K, L, M, N, NO
      character BLANK*1, LINE*120
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external PRIAM, LINER, SHIM, HI, BYE
C
C               STAB(N)
      dimension STAB(*)
C
      call HI ('GLEWY')
C     !BEG
      call PRIAM  (NO, 'R WAVELNGTHS', 12)
      write (NO,100)
  100 format(' ','Standard rates integrations wavelengths',64X,
     $           '(Option WTABPRNT)')
      call LINER  (1, NO)
C
      L = (N+3)/4
      do 104 K = 1,L
        LINE = BLANK
        M = -29
        I = K-L
        do 102 J = 1,4
          M = M+30
          I = I+L
          if(I.le.N) then
            write (LINE(M:M+29),101) I,STAB(I)
  101       format(I7,1PE23.15)
          end if
  102   continue
        write (NO,103) LINE
  103   format(' ',A)
        call SHIM (K, 5, NO)
  104 continue
C     !END
      call BYE ('GLEWY')
C
      return
      end
