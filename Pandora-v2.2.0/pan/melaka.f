      subroutine MELAKA
     $(NO,NCP,N,CORC)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Prints the Composite Line Opacity array.
C     (See also TAHAN.)
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
      external  PRIAM, DASHER, LINER, SHIM, ACHILES, HI, BYE
      intrinsic min
C
C               CORC(NCP,N)
      dimension CORC(NCP,*)
C
      data HUNDRD /1.D2/
C
      call HI ('MELAKA')
C     !BEG
      call PRIAM (NO,'COMP. CONT.',11)
      write (NO,100)
  100 format(' ','Relative contribution of Composite Line Opacity to ',
     $           'the total opacity, in percent.'//
     $       ' ','(The wavelengths are identified in the printout ',
     $           'section WAVELENGTHS, printed earlier.)'//
     $       ' ','For a compressed printout (120 wavelengths across), ',
     $           'set KCOAA = 1.')
C     !EJECT
      IE = 0
  101 continue
C
        IS = IE+1
        IE = min((IE+40),NCP)
        KT = IE-IS+1
        call LINER         (1,NO)
        call DASHER        (NO)
        write (NO,102) (I,I=(IS+9),IE,10)
  102   format(' ','depth',2X,'wave #',I24,3I30)
        write (NO,103) (PERIOD,I=(IS+9),IE,10)
  103   format(' ',7X,4(29X,A1))
        call LINER         (1,NO)
C
        LZ = 0
        do 107 J = 1,N
C
          M    = -2
          L    = 0
          LINE = BLANK
          do 104 I = IS,IE
            K = CORC(I,J)*HUNDRD
            M = M+3
            if(K.ge.100) then
              LINE(M:M+2) = ' **'
            else if(K.ge.0) then
              call ACHILES (K,LINE(M:M+2))
            end if
            if(K.le.0) then
              L = L+1
            end if
  104     continue
C
          if(L.eq.KT) then
            LZ   = LZ+1
            ZINE = LINE
          else
            if(LZ.gt.0) then
              if(LZ.eq.1) then
                write (NO,106) (J-1),ZINE
              else
                write (NO,105) LZ
  105           format(' ',I10,' lines of 0s omitted')
              end if
              LZ = 0
            end if
            write (NO,106) J,LINE
  106       format(' ',I5,2X,A120)
          end if
C
          call SHIM        (J,5,NO)
  107   continue
      if(IE.lt.NCP) goto 101
C     !END
      call BYE ('MELAKA')
C
      return
      end
