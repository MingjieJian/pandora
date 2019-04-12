      subroutine NOTES
     $(LU,N,NL,TIJ)
C
C     Rudolf Loeser, 1998 Jun 23
C---- Prints TIJ, for SICCUR.
C     !DASH
      save
C     !DASH
      real*8 CRIT, ONE, TIJ
      integer I, IJ, J, KNT, KO, L, LI, LIM, LU, M, N, NL
      logical ALLONES
      character BLANK*1, KINE*128, LINE*128
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  SETON, LINER, SHIM, CLOSUN, HI, BYE
      intrinsic abs
C
C               TIJ(N,NL**2)
      dimension TIJ(N,*)
C
      dimension IJ(10,3)
C
      data CRIT /1.D-4/
C
      call HI ('NOTES')
C     !BEG
      LIM = NL*(NL-1)
C     !EJECT
      do 107 J = 1,LIM,10
        call SETON        (J,NL,IJ,KNT)
        LINE = BLANK
        L = -3
        do 101 M = 1,KNT
          L = L+12
          write (LINE(L:),100) IJ(M,1),IJ(M,2)
  100     format(2X,'DIJ(',I2,'/',I2,')')
  101   continue
        call LINER        (1,LU)
        write (LU,102) LINE(2:)
  102   format(' ',A127)
        call LINER        (1,LU)
        LI = 0
        KO = 0
        I  = 0
  103   continue
          I = I+1
          if(I.le.N) then
            KINE = LINE
            write (LINE,104) I
  104       format(I8,120X)
            ALLONES = .true.
            L = -3
            do 105 M = 1,KNT
              L = L+12
              ALLONES = ALLONES.and.(abs(TIJ(I,IJ(M,3))-ONE).lt.CRIT)
              call CLOSUN (TIJ(I,IJ(M,3)),LINE(L+1:L+11))
  105       continue
            if(((I.gt.1).and.(I.lt.N)).and.ALLONES) then
              KO = KO+1
              goto 103
            end if
            if(KO.gt.1) then
              write (LU,106) KO
  106         format(' ',9X,'[',I5,' rows of ones omitted]')
              LI = LI+1
              call SHIM   (LI,5,LU)
            else if(KO.eq.1) then
              write (LU,102) KINE(:2)
              LI = LI+1
              call SHIM   (LI,5,LU)
            end if
            write (LU,102) LINE(2:)
            LI = LI+1
            KO = 0
            call SHIM     (LI,5,LU)
            goto 103
          end if
  107 continue
C     !END
      call BYE ('NOTES')
C
      return
      end
