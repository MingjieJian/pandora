      subroutine PLUTO
     $(XCBL,MRP,RNU,RCP,DOJN,YW,TR,N,XNU,XNUC,JLEV,YWS,TRS,LU)
C
C     Rudolf Loeser, 1972 May 17
C---- Establishes a complete set of YW and TR tables for level JLEV,
C     and provides summary printout if needed.
C     !DASH
      save
C     !DASH
      real*8 FN, RCP, RNU, TR, TRS, XCBL, XLM, XNU, XNUC, YW, YWS
      integer IPEX, J, JLEV, KKJNU, KKLTIT, KKTAUK, LU, LUEO, MRP, N
      logical DOJN
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK( 1),KKLTIT)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MONAD, NOMAD, MAREK, MOVE1, VITRY, BRIT, MESHED, MASHED,
     $         HI, BYE
C
C               RNU(MRX+1), YW(N,MRX+1), TR(N,MRX+1), XNU(NSL), YWS(N),
      dimension RNU(*),     YW(N,*),     TR(N,*),     XNU(*),   YWS(*),
C
C               TRS(N), RCP(MRX+1), XCBL(Miklen), XNUC(NSL)
     $          TRS(*), RCP(*),     XCBL(*),      XNUC(*)
C
      call HI ('PLUTO')
C     !BEG
      if(DOJN) then
        if((IPEX.lt.0).or.(IPEX.eq.12)) then
          call MESHED ('PLUTO', 3)
          write (LUEO,100) JLEV,MRP
  100     format(' ','JLEV =',I6,', MRP =',I10)
          if(MRP.ge.10) then
            write (LUEO,101) 'first',(RNU(J),J=1,10)
            write (LUEO,101) 'last ',(RNU(J),J=(MRP-9),MRP)
  101       format(' ',A5,2X,10F12.5)
          else
            write (LUEO,101) 'all  ',(RNU(J),J=1,MRP)
          end if
          call MASHED ('PLUTO')
        end if
C
C----   Loop over all frequencies for this level
        do 102 J = 1,MRP
C----     Convert RNU to FNU
          call MONAD  (RNU(J), XNU, XNUC, JLEV, FN )
C----     Convert RNU to LAMBDA
          call NOMAD  (RNU(J), XNU, XNUC, JLEV, XLM)
C----     Get and save Jnu
          call MAREK  (XLM, XCBL)
          call MOVE1  (XCBL(KKJNU), N, YW(1,J))
C----     Produce summary printout
          call VITRY  (LU, JLEV, J, XLM, RNU(J), RCP(J), N,
     $                 XCBL(KKTAUK), XCBL(KKJNU), XCBL(KKLTIT))
C----     Now compute TR from this Jnu
          call BRIT   (FN, N, YW(1,J), TR(1,J))
  102   continue
C
C----   Save first Jnu and TR, for plotting
        call MOVE1    (YW, N, YWS)
        call MOVE1    (TR, N, TRS)
      end if
C     !END
      call BYE ('PLUTO')
C
      return
      end
