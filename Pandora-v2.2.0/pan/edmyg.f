      subroutine EDMYG
     $(N,IU,IL,NED,DRHO,RHO,CAU1,MODE,KODE)
C
C     Rudolf Loeser, 1984 Jan 19
C---- Edits, for SOPHRON.
C
C     If the input value of MODE=1, then EDMYG only checks to see
C     whether editing is needed, returning KODE=1 if yes, =0 if no.
C     If MODE=2, does actual editing.
C     !DASH
      save
C     !DASH
      real*8 CAU1, DRHO, ONE, OPD, RHO
      integer I, IC, IL, IU, JU, KODE, MODE, N, NED
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               RHO(N), CAU1(N,NL)
      dimension RHO(*), CAU1(*)
C
      call HI ('EDMYG')
C     !BEG
      OPD = DRHO-ONE
      JU  = 1+N*(IU-1)
      IC  = ((JU-1)+NED)-1
C
      do 100 I = NED,N
        IC = IC+1
        if(RHO(I).lt.-CAU1(IC)) then
          if(MODE.eq.1) then
            KODE = 1
            goto 101
          else if(MODE.eq.2) then
C
            RHO(I) = OPD*CAU1(IC)
C
          end if
        end if
  100 continue
  101 continue
C     !END
      call BYE ('EDMYG')
C
      return
      end
