      subroutine MENAI
     $(TE,N,LU,DOTHEM,JQ,JX)
C
C     Rudolf Loeser, 1998 Mar 16
C---- Sets up diffusion plots controls, for CONSUL.
C     !DASH
      save
C     !DASH
      real*8 CRIT, TE
      integer I, JQ, JX, LIM, LODCG, LU, N, NODCG
      logical DOTHEM
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(158),LODCG)
      equivalence (KZQ(109),NODCG)
C     !DASH
      external HALA, KUPHAR, HI, BYE
C
C               TE(N)
      dimension TE(*)
C
      data CRIT /7.5D3/
C
      call HI ('MENAI')
C     !BEG
      DOTHEM = NODCG.ne.0
C
      if(DOTHEM) then
        do 100 I = 1,N
          LIM = I
          if(TE(I).lt.CRIT) goto 101
  100   continue
  101   continue
C
        call HALA   (LODCG, NODCG, LIM, JQ, JX)
C
        call KUPHAR (LU, LODCG, JQ, NODCG, JX)
      end if
C     !END
      call BYE ('MENAI')
C
      return
      end
