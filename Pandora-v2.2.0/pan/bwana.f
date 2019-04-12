      subroutine BWANA
     $(NL,LR,NO,RKCJ,TKRJ,YKRJ,J304I,M304)
C
C     Rudolf Loeser, 1972 Mar 02
C---- Prints RKCJ and CKRJ.
C     !DASH
      save
C     !DASH
      real*8 RKCJ, TKRJ, YKRJ
      integer I, IE, J304I, LI, LR, M304, NL, NO, jummy
      logical KILROY
      character TIT*3
C     !DASH
      external MINK, PADMA, LINER, STONE, HI, BYE
C
C               RKCJ(MLS), TKRJ(MLS), YKRJ(MLS), LR(NL)
      dimension RKCJ(*),   TKRJ(*),   YKRJ(*),   LR(*)
C
      dimension TIT(3)
C
      data TIT    /'TKR', 'YKR', 'RKC'/
      data KILROY /.true./
C
      call HI ('BWANA')
C     !BEG
      if(NO.gt.0) then
C
        do 101 I = 1,NL
          call MINK      (I, LR, IE, jummy)
          LI = LR(I)
          if(LI.gt.0) then
            if(KILROY) then
              KILROY = .false.
              call PADMA (NO, 'Additional Photoionization')
              write (NO,100)
  100         format(' ','TKR: Wavelength of ionizing radiation'/
     $               ' ','YKR: C.S.F. method control parameter'/
     $               ' ','RKC: Exponential term multiplier')
            end if
            call LINER   (1, NO)
            call STONE   (NO, I, LI, TKRJ(IE), TIT(1))
            call STONE   (NO, I, LI, YKRJ(IE), TIT(2))
            call STONE   (NO, I, LI, RKCJ(IE), TIT(3))
          end if
  101   continue
C
        if(J304I.gt.0) then
          call LINER     (1, NO)
          write (NO,102) M304
  102     format(' ','M304',I5)
        end if
C
      end if
C     !END
      call BYE ('BWANA')
C
      return
      end
