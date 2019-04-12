      subroutine MASULA
     $(NO,N,CCHX,ICXDP,MCXK,IQCXD,XRKH,XRLH,XRK,XRL)
C
C     Rudolf Loeser, 1990 Nov 23
C---- Prints upper-level charge exchange calculation data.
C     !DASH
      save
C     !DASH
      real*8 CCHX, XRK, XRKH, XRL, XRLH
      integer ICXDP, IQCXD, K, LL, LR, MCXK, N, NN, NO
      logical PRNTZ, ZR
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external PADMA, LINER, NAUGHTD, OMAR, HI, BYE
C
C               XRKH(N,NPQLM,NXI), XRLH(N,NPQLM,NXI), XRL(N,NPQLM),
      dimension XRKH(N,NPQLM,*),   XRLH(N,NPQLM,*),   XRL(*),
C
C               XRK(N,NPQLM)
     $          XRK(*)
C
      data PRNTZ /.false./
C     !EJECT
C
      call HI ('MASULA')
C     !BEG
      if((NO.gt.0).and.(MCXK.ne.0)) then
        call PADMA (NO,'Upper-Level Charge Exchange')
        if(MCXK.gt.0) then
          write (NO,100) NAMXED(MCXK), CCHX
  100     format(' ','For ',A//
     $           ' ','Charge exchange cross-section multiplier',
     $               T60,' CCHX =',1PE14.6)
          if(IQCXD.gt.0) then
            write (NO,101) ICXDP
  101       format(' ','Depth index for detailed dump printout',
     $                 T60,'ICXDP =',I4)
          end if
        else
C
          LR = 0
          LL = 0
          if(IQCXD.gt.0) then
            write (NO,102)
  102       format(' ','(Component input tables are printed because ',
     $                 'option CHXDMP is ON.)')
C
            NN = N*NPQLM
            do 104 K = 1,NXI
C
              call NAUGHTD (XRKH(1,1,K),1,NN,ZR)
              if(.not.ZR) then
                LR = LR+1
                call LINER (3,NO)
                write (NO,103) 'XRKH', NAMXED(K)
  103           format(' ',A,' from ',A)
                call OMAR  (NO,N,NPQLM,XRKH(1,1,K),'Level ',PRNTZ)
              end if
C
              call NAUGHTD (XRLH(1,1,K),1,NN,ZR)
              if(.not.ZR) then
                LL = LL+1
                call LINER (3,NO)
                write (NO,103) 'XRLH', NAMXED(K)
                call OMAR  (NO,N,NPQLM,XRLH(1,1,K),'Level ',PRNTZ)
              end if
C
  104       continue
            call LINER     (2,NO)
          end if
C     !EJECT
          if(LR.eq.1) then
            call LINER     (1,NO)
            write (NO,105) 'XRK'
  105       format(' ','(Total ',A,' not printed because there is ',
     $                 'only one component table, ',
     $                 'which was printed above.)')
          else
            call LINER     (3,NO)
            write (NO,106) 'XRK'
  106       format(' ','Total ',A)
            call OMAR      (NO,N,NPQLM,XRK,'Level ',PRNTZ)
          end if
          if(LL.eq.1) then
            call LINER     (1,NO)
            write (NO,105) 'XRL'
          else
            call LINER     (3,NO)
            write (NO,106) 'XRL'
            call OMAR      (NO,N,NPQLM,XRL,'Level ',PRNTZ)
          end if
        end if
      end if
C     !END
      call BYE ('MASULA')
C
      return
      end
