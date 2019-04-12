      subroutine TAHAN
     $(NO,NCP,N,CORC)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Prints the Composite Line Opacity array in abbreviated form.
C     (See also MELAKA.)
C     !DASH
      save
C     !DASH
      real*8 CORC, HUNDRD
      integer I, IE, IS, J, K, KT, L, LZ, M, N, NCP, NO
      character BLANK*1, LINE*120, PERIOD*1, ZINE*120
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external  PRIAM, DASHER, LINER, SHIM, IPOH, HI, BYE
      intrinsic min
C
C               CORC(NCP,N)
      dimension CORC(NCP,*)
C
      data HUNDRD /1.D2/
C
      call HI ('TAHAN')
C     !BEG
      call PRIAM (NO,'COMP. CONT.',11)
      write (NO,100)
  100 format(' ','Relative contribution of Composite Line Opacity to ',
     $           'the total opacity, in percent.'//
     $       ' ','(The wavelengths are identified in the printout ',
     $           'section WAVELENGTHS, printed earlier.)'//
     $       ' ','For more details (only 40 wavelengths across), ',
     $           'set KCOAA = 0 (default).')
      call LINER (1,NO)
      write (NO,101)
  101 format(' ','The numbers indicate percent/10, truncated. ',
     $           '"a" means at least 6 %, but less than 10 %;'/
     $       ' ','"m" means at least 3 %, but less than 6 %; ',
     $           '"z" means at least 1 %, but less than 3 %.')
C     !EJECT
      IE = 0
  102 continue
C
        IS = IE+1
        IE = min((IE+120),NCP)
        KT = IE-IS+1
        call LINER         (1,NO)
        call DASHER        (NO)
        write (NO,103) (I,I=(IS+9),IE,10)
  103   format(' ','depth',2X,'wave #',I4,11I10)
        write (NO,104) (PERIOD,I=(IS+9),IE,10)
  104   format(' ',7X,12(9X,A1))
        call LINER         (1,NO)
C
        LZ = 0
        do 108 J = 1,N
C
          M    = 0
          L    = 0
          LINE = BLANK
          do 105 I = IS,IE
            K = CORC(I,J)*HUNDRD
            M = M+1
            if(K.gt.0) then
              call IPOH (K,LINE(M:M))
            else
              L = L+1
            end if
  105     continue
C
          if(L.eq.KT) then
            LZ   = LZ+1
            ZINE = LINE
          else
            if(LZ.gt.0) then
              if(LZ.eq.1) then
                write (NO,107) (J-1),ZINE
              else
                write (NO,106) LZ
  106           format(' ',I10,' lines of blanks omitted')
              end if
              LZ = 0
            end if
            write (NO,107) J,LINE
  107       format(' ',I5,2X,A120)
          end if
C
          call SHIM        (J,5,NO)
  108   continue
      if(IE.lt.NCP) goto 102
C     !END
      call BYE ('TAHAN')
C
      return
      end
