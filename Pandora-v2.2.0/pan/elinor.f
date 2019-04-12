      subroutine ELINOR
     $(N,NL,HND,RABD,ABDEL,FRN,XNK,XND,KOUNT,FOLD,IMG)
C
C     Rudolf Loeser, 1998 Feb 06
C---- Renormalizes number densities.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, FOLD, FRN, HND, RABD, VAL, XDEN, XND, XNK, XNUM,
     $       ZERO
      integer I, IMG, J, KOUNT, N, NL
      logical BAD
C     !COM
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- ESPY        as of 2004 May 18
      logical     ESPION
      common      /ESPY/ ESPION
C     "Values range" constraint switch
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ROWSUM, DIVIDE, SNUFFLE, FOOZLE, HI, BYE
C
C               XND(N,NL), ABDEL(N), RABD(N), HND(N), FRN(N), FOLD(N),
      dimension XND(N,*),  ABDEL(*), RABD(*), HND(*), FRN(*), FOLD(*),
C
C               XNK(N), IMG(N)
     $          XNK(*), IMG(*)
C
      call HI ('ELINOR')
C     !BEG
      call ROWSUM   (XND, N, N, 1, NL, FRN)
C
      BAD = .false.
      do 100 I = 1,N
        XNUM = (ABDEL(I)*RABD(I))*HND(I)
        XDEN = XNK(I)+FRN(I)
        if(XDEN.le.ZERO) then
          BAD = .true.
        end if
        call DIVIDE (XNUM, XDEN, FRN(I))
  100 continue
C     !EJECT
      if(.not.BAD) then
        do 102 I = 1,N
          VAL = FRN(I)*XNK(I)
          if(ESPION) then
            call SNUFFLE   (VAL, ZZLALT, ZZSALT, XNK(I))
          else
            XNK(I) = VAL
          end if
C
          do 101 J = 1,NL
            VAL = FRN(I)*XND(I,J)
            if(ESPION) then
              call SNUFFLE (VAL, ZZLALT, ZZSALT, XND(I,J))
              if(XND(I,J).ne.VAL) then
                KOUNT = KOUNT+1
              end if
            else
              XND(I,J) = VAL
            end if
  101     continue
  102   continue
C
        if(ESPION) then
          call FOOZLE      (XND, XNK, NL, N, ZZSALT, FOLD, IMG)
        end if
      end if
C     !END
      call BYE ('ELINOR')
C
      return
      end
