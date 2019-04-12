      subroutine SOD
     $(NL,KIJ,XND,SET,XQ,P,TND,EDITED,ITAU,DUMP,KILROY,CALLER)
C
C     Rudolf Loeser, 2003 Apr 25
C---- Edits number densities.
C     !DASH
      save
C     !DASH
      real*8 FAC, ONE, P, PRAT, SET, TND, XND, XQ, dummy
      integer IL, IMAX, IMIN, ITAU, IU, KIJ, KODE, LODE, NL, jummy
      logical DUMP, EDITED, KILROY
      character CALLER*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, CRANKY, MINMAXD, DIVIDE, NOBLE, HI, BYE
C
C               XND(NL), TND(NL), XQ(NL), P(NL), KIJ(NL,NL), SET(N,MUL)
      dimension XND(*),  TND(*),  XQ(*),  P(*),  KIJ(*),     SET(*)
C
      data FAC /1.01D0/
C     !EJECT
C
      call HI ('SOD')
C     !BEG
      KODE = 0
      if(NL.gt.1) then
        do 101 IL = (NL-1),1,-1
          call ZERO1       (TND, NL)
          call ZERO1       (XQ , NL)
          LODE = 0
          do 100 IU = (IL+1),NL
C
            call CRANKY    (ITAU, IU, IL, KIJ, SET, XQ(IU))
            if(XQ(IU).le.ONE) then
              LODE = LODE+1
              call DIVIDE  (P(IL), P(IU), PRAT)
              TND(IU) = (FAC*XND(IU))*PRAT
              if(DUMP) then
                call NOBLE (LODE, NL, IL, XND, P, IU, XQ(IU),
     $                      TND(IU), ITAU, KILROY, CALLER)
              end if
            end if
C
  100     continue
          if(LODE.gt.0) then
            KODE = KODE+1
            call MINMAXD   (TND, 1, NL, IMIN, IMAX)
            XND(IL) = TND(IMAX)
            if(DUMP) then
              call NOBLE   (0, NL, jummy, XND, dummy, jummy, dummy,
     $                      dummy, ITAU, KILROY, CALLER)
            end if
          end if
  101   continue
      end if
      EDITED = KODE.gt.0
C     !END
      call BYE ('SOD')
C
      return
      end
