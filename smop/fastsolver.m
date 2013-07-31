function moves=solver(A,B,w0)
[moves,optmove,optscore]=cbest(A,B,w0);
curscore=sum(w0(moves(:,1)));
lots=1;
if length(moves)-optmove<20||curscore/optscore<1.05
    lots=2; return
else
    lenw=length(w0);
    [xx,nseq]=sort(rand(1,lenw));
    A1=A;
    B1=B;
    w01=w0;
    for i=1:lenw
        A1(A==i)=nseq(i);
        B1(B==i)=nseq(i);
        w01(nseq(i))=w0(i);
    end;
    [moves2,optmove,optscore]=cbest(A1,B1,w01);
    moves22=moves2;
    for i=1:lenw
        moves22(moves2(:,1)==nseq(i),1)=i;
    end;
    curscore2=sum(w0(moves22(:,1)));
    if curscore2<curscore
        moves=moves22;
    else
     %   [moves1,yess]=dealWall1(A,B,w0,moves);
    end;
end;
% final clean up before passing off
n=length(w0);
ipos1=zeros(n,1);
ipos2=ipos1;
fpos1=ipos1;
fpos2=ipos1;
for i=1:n
    [ipos1(i) ipos2(i)]=find(A==i);
    [fpos1(i) fpos2(i)]=find(B==i);
end
optmove=sum(abs(fpos1-ipos1)+abs(fpos2-ipos2));
optscore= sum((abs(fpos1-ipos1)+abs(fpos2-ipos2)).*w0);
moves=improve(A,B,w0,optmove,optscore,[ipos1 ipos2],[fpos1 fpos2],moves,lots);
%%%%%
function [moves,optmove,optscore]=cbest(A,B,w0)
lots=0;
n=length(w0);
ipos1=zeros(n,1);
ipos2=ipos1;
fpos1=ipos1;
fpos2=ipos1;
for i=1:n
    [ipos1(i) ipos2(i)]=find(A==i);
    [fpos1(i) fpos2(i)]=find(B==i);
end
optmove=sum(abs(fpos1-ipos1)+abs(fpos2-ipos2));
optscore= sum((abs(fpos1-ipos1)+abs(fpos2-ipos2)).*w0);score=inf;
if n~=28||(numel(A)/n)<=1.98
    mv=TLL79(A,B,w0,optmove,optscore);
    movesTLL79=improve(A,B,w0,optmove,optscore,[ipos1 ipos2],[fpos1 fpos2],mv,lots);
    scoreTLL79=sum(w0(movesTLL79(:,1)));
    if (scoreTLL79-optscore)>0;
        movesA5=improve(A,B,w0,optmove,optscore,[ipos1 ipos2],[fpos1 fpos2],itTakesAThief(A,B,w0),lots);
        scoreA5=sum(w0(movesA5(:,1)));
        if scoreTLL79<scoreA5,
            moves=movesTLL79;
        else
            moves=movesA5;
        end
    else;
        moves=movesTLL79;
    end;
    flag =0;
else
    flag=1;
end
if ((n>=18&&n<=20)||n==28||n==23||(n>=13&&n<=16)) &&...
        (flag||length(moves)>1.18*optmove)&&(numel(A)/n)>1.98
    if ((score-optscore)<20)||(~flag&&(length(moves)-optmove)<2);return;end;
    ym=size(A,1);
    M=[ym 1 -ym -1];
    mask=true(1,n);
    w=abs(w0(:)')+.1;
    rand('seed',100);
    [mov,ok]=mainsolver(A,B,w,mask);
    mov=1*(mov==M(1))+2*(mov==M(2))+3*(mov==M(3))+4*(mov==M(4));
    mov=[(mov>0)*(1:n)' sum(mov,2)];
    mov=improve(A,B,w0,optmove,optscore,[ipos1 ipos2],[fpos1 fpos2],mov,lots);
    if ok&&flag==0
        if sum(w0(mov(:,1)))<sum(w0(moves(:,1)))
            moves=mov;
        end
    else
        moves=mov;
    end
end
%%%%%%%%%%%%%%%%%%
function [mov,ok]=mainsolver(A,B,w,mask)
n=sum(mask);
if n==0
    ok=0;
    return
end
[mov,ok]=easysolver(A,B,w,mask);
if ok
    return
end
ipos=zeros(size(mask));
for i=find(mask)
    ipos(i)=find(A==i);
end
invw=max(w)+1-w;
blockers=sum(findoverlaps(cumsum([ipos;mov])));
Apz=zeros(size(A));
Bpz=zeros(size(B));
for leaveout=ceil(2*log(n)):(n-1)
    mmask=mask;
    choosew=sum(blockers).*invw;
    for i=1:leaveout
        small=find(cumsum(mmask.*choosew)>=rand*sum(mmask.*choosew));
        mmask(small(1))=false;
    end
    Ap=Apz;
    Bp=Bpz;
    for i=find(mmask)
        Ap(ipos(i))=i;
        Bp(B==i)=i;
    end
    [partialmov,ok]=mainsolver(Ap,Bp,w+min(w)*rand(size(w)),mmask);
    blockers=blockers+sum(findoverlaps(cumsum([ipos;partialmov])));
    if ~ok
        continue
    end
    for i=find(mask)
        Ap(ipos(i))=i;
    end
    for i=1:3*sum(mask)
        small=find(cumsum(w.*(~mmask & mask))>=rand*sum(w.*(~mmask & mask)));
        [trymov,ok]=imoves(small(1),partialmov,Ap,B,mmask);
        if ok
	mmask(small(1))=true;
            if isequal(mmask,mask)
                ok=1;
                mov=trymov;
                return
            end
            partialmov=trymov;            
        else
            dropw=blockers.*invw;
            small=find(cumsum(dropw.*(mmask&mask))>=rand*sum(dropw.*(mmask&mask)));
            mmask(small(1))=false;
            partialmov(find(partialmov(:,small(1))),:)=[];
        end
    end
    ok=0;
end
%%%%%%%%%%%%%%%%%%
function [mov,ok]=imoves(c,mov,A,B,mask)
mov(find(mov(:,c)),:)=[];
nmov=size(mov,1);
[crow,ccol]=find(A==c);
[trow,tcol]=find(B==c);
if nmov==0&&crow==trow&&ccol==tcol
    ok=1;
    return
end
n=length(mask);

[rows,cols]=size(A);
slices=false(rows,cols,nmov+1);
ipos=zeros(1,n);
for i=find(mask)
    ipos(i)=find(A(:)==i);
end
pos=cumsum([ipos;mov],1);
for k=1:(nmov+1)
    R=zeros(size(A));
    R(pos(k,mask))=-1;
    slices(:,:,k)=(R==-1);
end
target=sub2ind(size(slices),trow,tcol,nmov+1);
optlen=abs(crow-trow)+abs(ccol-tcol);
nnnn=numel(slices);
path=dijkstra(nnnn,find(A==c),target,slices,(1+0.1*log(nnnn))*optlen);
if isempty(path)
    ok=0;
    return
end
for i=length(path):-1:2
    if path(i)-path(i-1)<rows*cols
        k=floor((path(i-1)-1)/(rows*cols))+1;
        mov=[mov(1:(k-1),:);zeros(1,n);mov(k:end,:)];
        mov(k,c)=path(i)-path(i-1);
    end
end
ok=1;
%%%%%%%%%%%%%%%%%%
function [moves,ok]=easysolver(A,B,w,mask)
global TEST
TEST=1;
NFIDDLE=5;
ok=0;
ym=size(A,1);
n=length(mask);
M=[ym 1 -ym -1];
moves=[];
ipos=zeros(1,n);
for i=find(mask)
    [row,col]=find(A==i);
    [rowt,colt]=find(B==i);
    nm=abs(col-colt)+abs(row-rowt);
    moves=[moves;zeros(nm,i-1) ...
        [M(1)*ones(max((colt-col),0),1);...
        M(2)*ones(max((rowt-row),0),1);...
        M(3)*ones(max((col-colt),0),1);...
        M(4)*ones(max((row-rowt),0),1)] ...
        zeros(nm,n-i)];
    ipos(i)=(col-1)*ym+row;
end
nmov=size(moves,1);
if nmov==0
    ok=1;
    return
end
moves=moves(randperm(nmov),:);
for i=1:NFIDDLE
    pos=cumsum([ipos;moves]);
    okmov=min(find(any(findoverlaps(pos),2)))-1;
    if isempty(okmov)
        ok=1;
        return
    end
    indperm=localfiddler(pos(okmov,:),moves(okmov:nmov,:),w);
    moves(okmov:nmov,:)=moves(okmov-1+indperm,:);
end
pos=cumsum([ipos;moves]);
small=find(any(findoverlaps(pos),2));
okmov=small(1)-1;
if isempty(okmov)
    ok=1;
    return
end
oldmoves=moves;
for i=1:ceil(sqrt(nmov-okmov))
    pos=cumsum([ipos;moves]);
    overlap=findoverlaps(pos);
    if ~any(overlap(:))
        ok=1;
        return
    else
        small=find(any(overlap,2));
        oli=small(1);
        small=find(overlap(oli,:));
        one=small(1);
        small=find(overlap(oli,:));
        other=small(end);
        rowinds=[oli-1 ...
            min(find(moves(:,one) & (1:nmov)'>=oli)) ...
            min(find(moves(:,other) & (1:nmov)'>=oli)) ...
            max(find(moves(:,one) & (1:nmov)'<oli-1)) ...
            max(find(moves(:,other) & (1:nmov)'<oli-1))];
        indperm=localfiddler(pos(oli-1,:),moves(rowinds,:),w);
        moves(rowinds,:)=moves(rowinds(indperm),:);
    end
end
pos=cumsum([ipos;moves]);
small=find(any(findoverlaps(pos),2));
if isempty(small)||(small(1)-1<okmov)
    moves=oldmoves;
end
%%%%%%%%%%%%%%%%%%
function indperm=localfiddler(ipos,mov,w)
M=4;
[nmov,n]=size(mov);
if nmov<=M
    M=nmov;
    P=perms(1:M);
else
    M=min(M,nmov);
    inds=(M+1):nmov;
    P=[perms(1:M) inds(ones(prod(2:M),1),:)];
    for i=1:size(P,1)
        P(i,M+1:nmov)=inds(randperm(nmov-M));
    end
end
f=zeros(size(P,1),1);
for i=1:size(P,1)
    newmov=mov(P(i,:),:);
    pos=cumsum([ipos;newmov]);
    overlap=findoverlaps(pos);
    if ~any(overlap(:))
        indperm=P(i,:);
        return
    else
        p=any(overlap,1);
        for c=1:n
            if p(c)
                small=find(overlap(:,c));
                f(i)=f(i)+w(c)*small(1);
            else
                f(i)=f(i)+w(c)*nmov;
            end
        end
    end
end
[DNC,best]=max(f);
indperm=P(best,:);
%%%%%%%%%%%%%%%%%%
function overlaps=findoverlaps(pos)
[npos,n]=size(pos);
cols=find(all(pos));
pos=pos(:,cols);
A=zeros(npos,length(cols));
[sortpos,ind]=sort(pos,2);
ind1=ind(:,1:end-1);
ind2=ind(:,2:end);
dp=diff(sortpos,1,2)==0;
for i=1:npos
	A(i,ind1(i,dp(i,:)))=true;
A(i,ind2(i,dp(i,:)))=true;
end
overlaps=zeros(npos,n);
overlaps(:,cols)=A;
%%%%%%%%%%%%%%%%%%
function [path]=dijkstra(n,s,d,R,optlen)
[rows,cols,depth]=size(R);
R=R(:);
visited=false(1,n);
distance=inf*ones(1,n);
parent=zeros(1,n);
distance(s)=0;
stack=zeros(1,n);
stack(1)=s;
next=1;
last=1;
for i=1:(n-1),
    if next>last
        break
    else
        u=stack(next);
        next=next+1;
    end
    visited(u)=true;
    ndx=u-1;
    k=floor(ndx/(rows*cols))+1;
    ndx=rem(ndx,rows*cols);
    col=floor(ndx/rows)+1;
    ndx=rem(ndx,rows);
    row=floor(ndx)+1;
    v=u+1;
    if v>0&&v<=n&&~R(v)&&row<rows
        if distance(u)+1<distance(v)
            distance(v)=distance(u)+1;
            parent(v)=u;
            if ~visited(v)
                if (v==d)&&(distance(d)==optlen)
                    break
                end
                last=last+1;
                stack(last)=v;
            end
        end
    end
    v=u-1;
    if v>0&&v<=n&&~R(v)&&row>1
        if distance(u)+1<distance(v)
            distance(v)=distance(u)+1;
            parent(v)=u;
            if ~visited(v)
                if (v==d)&&(distance(d)==optlen)
                    break
                end
                last=last+1;
                stack(last)=v;
            end
        end
    end
    v=u+rows;
    if v>0&&v<=n&&~R(v)&&col<cols
        if distance(u)+1<distance(v)
            distance(v)=distance(u)+1;
            parent(v)=u;
            if ~visited(v)
                if (v==d)&&(distance(d)==optlen)
                    break
                end
                last=last+1;
                stack(last)=v;
            end
        end
    end
    v=u-rows;
    if v>0&&v<=n&&~R(v)&&col>1
        if distance(u)+1<distance(v)
            distance(v)=distance(u)+1;
            parent(v)=u;
            if ~visited(v)
                if (v==d)&&(distance(d)==optlen)
                    break
                end
                last=last+1;
                stack(last)=v;
            end
        end
    end
    v=u+rows*cols;
    if v>0&&v<=n&&~R(v)&&k<depth
        if distance(u)<distance(v)
            distance(v)=distance(u);
            parent(v)=u;
            if ~visited(v)
                if (v==d)&&(distance(d)==optlen)
                    break
                end
                last=last+1;
                stack(last)=v;
            end
        end
    end
end
if parent(d)~=0
    path=zeros(1,distance(d)+depth);
    pathi=length(path);
    t=d;
    path(pathi)=d;
    pathi=pathi-1;
    while t~=s
        p=parent(t);
        path(pathi)=p;
        pathi=pathi-1;
        t=p;
    end
else
    path=[];
end
%%%%%
function mv=improve(Ai,Af,w,optmove,optscore,Ci,Cf,mv,lots)
dist=optscore;
n_blk=length(w);
I=[0  1  0 -1];
J=[1  0 -1  0];
sc=sum(w(mv(:,1)));mv_len=size(mv,1);
if sc==optscore;return;end;
if lots==1;
 if ((sc-optscore)<5)||((mv_len-optmove)<1);return;end
 num_fail=0;max_try=min(125,mv_len*2);max_fail=max_try;
elseif lots==0;
 if ((sc-optscore)<20)||((mv_len-optmove)<2);return;end
 num_fail=0;max_try=min(40,mv_len);max_fail=max_try;
elseif lots==2;
 if ((sc-optscore)<5)||((mv_len-optmove)<1);return;end
 num_fail=0;max_try=min(55,mv_len);max_fail=max_try;
end;
for j_try=1:max_try,
    sc_old=sc;A=Ai;C=Ci;
    j=1;
    while j<mv_len,
        if (mv(j,1)==mv(j+1,1))&&(abs(mv(j,2)-mv(j+1,2))==2),
            sc=sc-2*w(mv(j,1));
            mv=mv([1:j-1,j+2:mv_len],:);
            mv_len=mv_len-2;
            if sc==dist,return;end
        else
            if A(C(mv(j+1,1),1)+I(mv(j+1,2)),C(mv(j+1,1),2)+J(mv(j+1,2)))==0,
                mv([j j+1],:)=mv([j+1 j],:);
            else
                if j+3<=mv_len,
                    b=mv(j,1);c=mv(j+1,1);d=mv(j+2,1);
                    sit=0;
                    if (b==mv(j+3,1))&&(abs(mv(j,2)-mv(j+3,2))==2)
                        if (b==c)&&(b==d)&&(mv(j+1,2)==mv(j+2,2)),
                            b1=A(C(b,1)+I(mv(j+1,2)),C(b,2)+J(mv(j+1,2)));sit=1;
                        else
                            if (b==c)&&(b~=d)&&(abs(mv(j+1,2)-mv(j+2,2))==2),
                                b1=d;sit=1;
                            else
                                if (b~=c)&&(b==d)&&(abs(mv(j+1,2)-mv(j+2,2))==2),
                                    b1=c;sit=1;
                                else
                                    if (b==c)&&(b~=d),
                                        sc=sc-2*w(b);mv(j,1)=d;mv(j,2)=mv(j+2,2);
                                        mv=mv([1:j+1,j+4:mv_len],:);mv_len=mv_len-2;
                                    end
                                end
                            end
                        end
                    end
                    if sit==1,
                        if w(b1)<w(b),
                            mv(j,1)=b1;mv(j+3,1)=b1;
                            sc=sc-2*(w(b)-w(b1));
                        end
                    end
                end
            end
            if (sc-optscore)<5;return;end;
            b=mv(j,1);r=C(b,1);c=C(b,2);
            nr=r+I(mv(j,2));nc=c+J(mv(j,2));
            A(r,c)=0;A(nr,nc)=b;
            C(b,1)=nr;C(b,2)=nc;
            j=j+1;
        end
    end
    if sc_old<=sc,
        num_fail=num_fail+1;
        if num_fail==max_fail,
            break
        end
    else
        num_fail=0;
    end
end
%%%%%
function move=TLL79(aInit,aFinal,wt,optmove,optscore)
o=[3;4;1;2];
[move,score]=solverA(aInit,aFinal,wt,optmove,optscore,0);
if ((score-optscore)>20)||((length(move)-optmove)>3);
    [mv2,score2]=solverA(aFinal,aInit,wt,optmove,optscore,0);
    if score>score2
        move=mv2(end:-1:1,:);
        score=score2;
        move(:,2)=o(move(:,2));
    end
    if ((score-optscore)>20)||((length(move)-optmove)>3);
        a=aInit;
        midmoves=floor(size(move,1)/2);
        I=[0  1  0 -1];
        J=[1  0 -1  0];
        for k=1:midmoves
            [row,col]=find(a==move(k,1));
            a(row,col)=0;
            row=row+I(move(k,2));
            col=col+J(move(k,2));
            a(row,col)=move(k,1);
        end
        oldscore1=sum(wt(move(1:midmoves,1)));
        oldscore2=sum(wt(move(midmoves+1:size(move,1),1)));
        [newmove,newscore]=solverA(aInit,a,wt,optmove,optscore,1);
        [newmove2,newscore2]=solverA(a,aFinal,wt,optmove,optscore,1);
        newscore1=sum(wt(newmove(:,1)));
        newscore2=sum(wt(newmove2(:,1)));
        oldmove=move;
        if(newscore1<oldscore1)
            move=newmove;
            if(newscore2<oldscore2)
                for itr=1:size(newmove2,1)
                    move(end+1,[1 2])=[newmove2(itr,1) newmove2(itr,2)];
                end
            else
                for itr=midmoves+1:size(oldmove,1)
                    move(end+1,[1 2])=[oldmove(itr,1) oldmove(itr,2)];
                end
            end
        else
            if(newscore2<oldscore2)
                move=[];
                for itr=1:midmoves
                    move(end+1,[1 2])=[oldmove(itr,1) oldmove(itr,2)];
                end
                for itr=1:size(newmove2,1)
                    move(end+1,[1 2])=[newmove2(itr,1) newmove2(itr,2)];
                end
            end
        end
    end
end;
%%%%%%%%%%%%%%%%%%
function [move,score]=solverA(aInit,aFinal,wt,optmove,optscore,half)
global Dperfect
Dperfect=optmove;if half;optmove=1e20;optscore=1e20;end;
[move,isPerfect,score]=solver2(aInit,aFinal,wt,1111,optscore);
if ~isPerfect;
    move1=solver1(aInit,aFinal,wt,score);
    if (~isempty(move1))
        isPerfect=length(move1)==Dperfect;
        score1=sum(wt(move1(:,1)));
        if score1<score;
            score=score1;move=move1;
        end;
    end
end;
if isPerfect;score=0;end;
%%%%%%%%%%%%%%%%%%
function move=solver1(aInit,aFinal,wt,bscore)
COUNTERS=0;
allmoves =[];
wtinitori=wt;
sortbydist  =0;
sortbyweight=1;
minw=10000;
if sortbyweight
    [tmp inds]=sort(-wt);
    wtinit=wtinitori;
end;
lenwt=length(wt);
absx=zeros(lenwt,1);
absy=absx;
for index=1:lenwt
    [hvix hviy]=find(aInit ==index);
    [hvfx hvfy]=find(aFinal==index);
    absx(index)=abs(hvix-hvfx);
    absy(index)=abs(hviy-hvfy);
end;
if sortbydist
    dist=abs(absx)+abs(absy);
    [tmp inds]=sort(-dist);
    wtinit=wtinitori;
end;
for index=1:length(inds)
    hv=inds(index);
    [hvix hviy]=find(aInit ==hv);
    [hvfx hvfy]=find(aFinal==hv);
    dist=abs(hvix-hvfx)+abs(hviy-hvfy);
    wt(hv)=-Inf;
    wtinit(hv)=wtinit(hv)+minw+10000;
    [move cost aInit COUNTERS]=movefrompos(hv,[hvix hviy],[hvfx hvfy],aInit,aFinal,wtinit,1,dist+5,COUNTERS);
    if COUNTERS>500;move=[];return;end;
    if isinf(cost)||isempty(move)
    else
        move=[move(:,1) 3*abs(move(:,4))-move(:,4)+2*abs(move(:,5))-move(:,5)];
        allmoves=[allmoves;move];
        if (sum(wtinitori(allmoves(:,1)))>bscore)
            move=[];
            return;
        end
    end;
end;
count  =1;
oldinds=[];
while ~isequal(aFinal,aInit)
    sortbyweight=sortbydist;
    sortbydist  =~sortbyweight;
    if sortbyweight
        inds=find(aFinal~=aInit);
        inds=aFinal(inds);
        inds=inds(inds>0);
        [tmp indices]=sort(-wt(inds));
        inds=inds(indices);
    end;
    if sortbydist
        for index=1:length(wt)
            [hvix hviy]=find(aInit ==index);
            [hvfx hvfy]=find(aFinal==index);
            dist(index)=abs(hvix-hvfx)+abs(hviy-hvfy);
        end;
        [tmp inds]=sort(-dist);
        firstzeros=find(tmp==0);
        inds(firstzeros:end)=[];
    end;
    wtinit=wtinitori;
    notmove=setdiff(oldinds,inds);
    for hv=1:length(notmove)
        wtinit(notmove(hv))=wtinit(notmove(hv))+minw+10000;
    end;
    for hind=1:length(inds)
        hv=inds(hind);
        [hvix hviy]=find(aInit ==hv);
        [hvfx hvfy]=find(aFinal==hv);
        dist=abs(hvix-hvfx)+abs(hviy-hvfy);
        wtinit(hv)=wtinit(hv)+minw+10000;
        [move cost aInit COUNTERS]=movefrompos(hv,[hvix hviy],[hvfx hvfy],aInit,aFinal,wtinit,1,dist+6,COUNTERS);
        if COUNTERS>500;move=[];return;end;
        if hind>1
            wtinit(inds(hind-1))=wtinit(inds(hind-1))-minw-10000;
        end;
        if isinf(cost)||isempty(move)
        else
            move=[move(:,1) 3*abs(move(:,4))-move(:,4)+2*abs(move(:,5))-move(:,5)];
            allmoves=[allmoves;move];
            if (sum(wtinitori(allmoves(:,1)))>bscore)
                move=[];
                return;
            end
        end;
    end;
    oldinds=inds;
    count=count+1;
end;
move=allmoves;
%%%%%%%%%%%%%%%%%%
function [move,cost,curposnew,COUNTERS]=movefrompos(item,pinit,pfin,curpos,finpos,wt,strategy,recur,COUNTERS)
COUNTERS=COUNTERS+1;
cost=0;
curposnew=curpos;
if isequal(pinit,pfin),move=[];return;end;
if recur==0,move=[];cost=0;return;end;
diffx=pfin(1)-pinit(1);
diffy=pfin(2)-pinit(2);
dirx=sign(diffx);
diry=sign(diffy);
cost=Inf;
if (strategy<10&&abs(diffx)>abs(diffy))||(strategy==11&&abs(diffx)<abs(diffy))
    if dirx&&curpos(pinit(1)+dirx,pinit(2))<=0
        [move cost curposnew COUNTERS]=onemove(item,pinit,[dirx 0],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if length(move)>0&&cost/size(move,1)==mod(wt(curpos(pinit(1),pinit(2))),10000),return;end;
    end;
    if diry&&curpos(pinit(1),pinit(2)+diry)<=0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[0 diry],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if length(move1)>0&&cost1/size(move1,1)==mod(wt(curpos(pinit(1),pinit(2))),10000)
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
            return;
        end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
else
    if diry&&curpos(pinit(1),pinit(2)+diry)<=0
        [move cost curposnew COUNTERS]=onemove(item,pinit,[0 diry],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if length(move)>0&&cost/size(move,1)==mod(wt(curpos(pinit(1),pinit(2))),10000),return;end;
    end;
    if dirx&&curpos(pinit(1)+dirx,pinit(2))<=0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[dirx 0],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if length(move1)>0&&cost1/size(move1,1)==mod(wt(curpos(pinit(1),pinit(2))),10000)
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
            return;
        end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
end;
if isinf(cost)||cost==0
    if dirx&&diry&&curpos(pinit(1),pinit(2)+diry)>0&&curpos(pinit(1)+dirx,pinit(2))>0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[dirx 0],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[0 diry],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    else
        if dirx&&curpos(pinit(1)+dirx,pinit(2))>0
            [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[dirx 0],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
            if cost1<cost
                move=move1;
                cost=cost1;
                curposnew=curposnew1;
            end
        end;
        if diry&&curpos(pinit(1),pinit(2)+diry)>0
            [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,[0 diry],pfin,curpos,finpos,wt,strategy,recur,COUNTERS);if COUNTERS>500;move=[];return;end;
            if cost1<cost
                move=move1;
                cost=cost1;
                curposnew=curposnew1;
            end
        end;
    end;
end;
if isinf(cost)&&recur>1&&abs(strategy)<5
    mv =[dirx diry];
    mv1=abs(mv)-1;
    if any(mv1)
        mv2=abs(mv1);
        mv3=-mv;
        ok3=1;
    else
        mv1=[-mv(1) 0];
        mv2=[0 -mv(2)];
        ok3=0;
    end;
    sz =size(curpos);
    strategy=strategy+sign(strategy)*1;
    if all(pinit+mv1>0)&&all(pinit+mv1<=sz)&&~curpos(pinit(1)+mv1(1),pinit(2)+mv1(2))
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv1,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
    if all(pinit+mv2>0)&&all(pinit+mv2<=sz)&&~curpos(pinit(1)+mv2(1),pinit(2)+mv2(2))
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv2,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
    if ok3&&all(pinit+mv3>0)&&all(pinit+mv3<=sz)&&~curpos(pinit(1)+mv3(1),pinit(2)+mv3(2))
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv3,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
    if all(pinit+mv1>0)&&all(pinit+mv1<=sz)&&curpos(pinit(1)+mv1(1),pinit(2)+mv1(2))>0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv1,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
    if all(pinit+mv2>0)&&all(pinit+mv2<=sz)&&curpos(pinit(1)+mv2(1),pinit(2)+mv2(2))>0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv2,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;
    if ok3&&all(pinit+mv3>0)&&all(pinit+mv3<=sz)&&curpos(pinit(1)+mv3(1),pinit(2)+mv3(2))>0
        [move1 cost1 curposnew1 COUNTERS]=onemove(item,pinit,mv3,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);if COUNTERS>500;move=[];return;end;
        if cost1<cost
            move=move1;
            cost=cost1;
            curposnew=curposnew1;
        end
    end;

end;
if isinf(cost)
    move=[];
    curposnew=curpos;
end;
%%%%%%%%%%%%%%%%%%
function [move,cost,curpos,COUNTERS]=onemove(item,pinit,movedir,pfin,curpos,finpos,wt,strategy,recur,COUNTERS)
COUNTERS=COUNTERS+1;
move3=0;
curcost =mod(wt(curpos(pinit(1),pinit(2))),10000);
costnew3=0;
newpos  =pinit+movedir;
if curpos(newpos(1),newpos(2))>0
    if wt(curpos(newpos(1),newpos(2)))>8000
        cost=Inf;
        move=[];
        return;
    end;
    if wt(curpos(newpos(1),newpos(2)))>2*curcost&&recur>0&&strategy>0
        tmppos1= abs(movedir)-1  +pinit;
        tmppos2= abs(movedir)-1  +pinit;
        tmppos3=movedir+tmppos1;
        tmppos4=movedir+tmppos2;
        sz=size(curpos);
        wt2      =[0;wt];
        curpos2=curpos;
        curpos2(curpos2<0)=0;
        curpos2=curpos2+1;
        tmpcost=[Inf Inf];
        if all(tmppos1>0)&&all(tmppos1<=sz)
            if all(tmppos3>0)&&all(tmppos3<=sz)
                tmpcost(1)=wt2(curpos2(tmppos1(1),tmppos1(2)))+wt2(curpos2(tmppos1(1),tmppos1(2)))+2*curcost;
            end;
        end;
        tmpcost(2)=tmpcost(1);
        if tmpcost(2)<wt(curpos(newpos(1),newpos(2)))
            [move3 costnew3 curpostmp COUNTERS]=onemove(item,pinit,tmppos2-pinit,pfin,curpos,finpos,wt,strategy,-1,COUNTERS);
            if ~isinf(costnew3)
                [move4 costnew4 curpostmp COUNTERS]=onemove(item,tmppos2,tmppos4-tmppos2,pfin,curpostmp,finpos,wt,strategy,-1,COUNTERS);
                if ~isinf(costnew4)
                    [move5 costnew5 curpostmp COUNTERS]=movefrompos(item,tmppos4,pfin,curpostmp,finpos,wt,-strategy,recur-1,COUNTERS);
                    cost=costnew3+costnew4+costnew5;
                    move=[move3;move4;move5];
                    if ~isinf(cost),curpos=curpostmp;end;
                    return;
                end;
            end;
        end;
    end;
    newitem=curpos(newpos(1),newpos(2));
    pfin2 = []
    [pfin2(1) pfin2(2)]=find(finpos==newitem);
    dirx2=sign(pfin2(1)-newpos(1));
    diry2=sign(pfin2(2)-newpos(2));
    wt(newitem)=wt(newitem)+20000;
    if isequal([dirx2 diry2],-movedir)||isequal(newpos,pfin2)
        costnew3=Inf;
    else
        [move3 costnew3 curposnew COUNTERS]=movefrompos(newitem,newpos,pfin2,curpos,finpos,wt,strategy,-1,COUNTERS);
        if costnew3==0,costnew3=Inf;end;
        if ~isinf(costnew3),curpos=curposnew;end;
    end;
    if isinf(costnew3)
        wtdir=[Inf Inf Inf Inf];
        diffpos=pfin-newpos;
        if (diffpos(1)&&movedir(1))||(diffpos(2)&&movedir(2))
            mv1=abs(movedir)-1;
        else
            if any(sign(diffpos)>0)
                mv1=abs(movedir)-1;
            else
                mv1=abs(abs(movedir)-1);
            end;
        end;
        mv2=-mv1;
        mv3=movedir;
        sz =size(curpos);
        if all(newpos+mv1>0)&&all(newpos+mv1<=sz)

            if curpos(newpos(1)+mv1(1),newpos(2)+mv1(2))<=0
                [move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv1,curpos,finpos,wt,strategy,-1,COUNTERS);
            else wtdir(1)=wt(curpos(newpos(1)+mv1(1),newpos(2)+mv1(2)));
            end;
        end;
        if isinf(costnew3)&&all(newpos+mv2>0)&&all(newpos+mv2<=sz)
            if curpos(newpos(1)+mv2(1),newpos(2)+mv2(2))<=0
                [move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv2,curpos,finpos,wt,strategy,-1,COUNTERS);
            else wtdir(2)=wt(curpos(newpos(1)+mv2(1),newpos(2)+mv2(2)));
            end;
        end;
        if isinf(costnew3)&&all(newpos+mv3>0)&&all(newpos+mv3<=sz)
            if curpos(newpos(1)+mv3(1),newpos(2)+mv3(2))<=0
                [move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv3,curpos,finpos,wt,strategy,-1,COUNTERS);
            else wtdir(3)=wt(curpos(newpos(1)+mv3(1),newpos(2)+mv3(2)));
            end;
        end;
        if isinf(costnew3)
            [wtdir si]=sort(wtdir);
            for index=1:length(wtdir)
                if ~isinf(wtdir(index))&&isinf(costnew3)
                    switch si(index),
                        case 1,[move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv1,curpos,finpos,wt,strategy,-1,COUNTERS);
                        case 2,[move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv2,curpos,finpos,wt,strategy,-1,COUNTERS);
                        case 3,[move3 costnew3 curpos COUNTERS]=movefrompos(newitem,newpos,newpos+mv3,curpos,finpos,wt,strategy,-1,COUNTERS);
                    end;
                end;
            end;
        end;
    end;
    wt(newitem)=wt(newitem)-20000;
    if isinf(costnew3)||costnew3==0
        cost=Inf;
        move=[];
        return;
    end;
end;
if curpos(newpos(1),newpos(2))==-item
    cost=Inf;
    move=[];
    return;
end;
curpos(newpos(1),newpos(2))= curpos(pinit(1),pinit(2));
curpos(pinit(1) ,pinit(2))=-curpos(pinit(1),pinit(2));
if recur>0
    [move2 costnew curposnew,COUNTERS]=movefrompos(item,pinit+movedir,pfin,curpos,finpos,wt,strategy,recur-1,COUNTERS);
else
    [move2 costnew curposnew,COUNTERS]=movefrompos(item,pinit+movedir,pfin,curpos,finpos,wt,strategy,0,COUNTERS);
end;
curpos=curposnew;
if curpos(pinit(1),pinit(2))<0
    curpos(pinit(1),pinit(2))=0;
end;
if numel(move3)==1&&isempty(move2);
    move=[item pinit movedir];
elseif numel(move3)==1
    move=[item pinit movedir;move2];
elseif isempty(move2)
    move=[move3;item pinit movedir];
else;
    move=[move3;item pinit movedir;move2];
end;
cost=costnew3+costnew+curcost;
%%%%%%%%%%%%%%%%%%
function [mv,perfectMV,score]=solver2(ai,af,w,states,optscore)
global Ac Ar m2 Dperfect
perfectMV=false;
nBlocks=length(w);
[m,n]=size(ai);
m2=m+2;
n2=n+2;
A=-ones(m2,n2);
Af=A;
A(2:m+1,2:n+1)=ai;
Af(2:m+1,2:n+1)=af;
Ac=1:n2;
Ac=Ac(ones(m2,1),:);
Ar=(1:m2)';
Ar=Ar(:,ones(n2,1));
Pi=w;
Pf=w;
for i=m2+2:numel(A)-m2-1
    if A(i)>0,Pi(A(i))=i;end
    if Af(i)>0,Pf(Af(i))=i;end
end
P=Pi;
nmv=1;
mv=zeros(300,2);
nNOK=sum(P~=Pf);
Paths=zeros(m+n,nBlocks);
lPaths=w;
fPaths=w;
Pend=w;
bOK=w;
obs=zeros(nBlocks,2);
nmv0=0;
while nmv0<nmv&&nNOK
    obs(:)=0;
    nmv0=nmv;
    for i=1:nBlocks
        if P(i)==Pf(i)
            lPaths(i)=0;
            fPaths(i)=0;
            bOK(i)=1;
        else
            [P1,f1]=SearchPath(A,P(i),Pf(i));
            if isempty(P1)
                lPaths(i)=0;
                fPaths(i)=0;
                obs(i,1:length(f1))=f1;
            else
                if isempty(f1)
                    fPaths(i)=1;
                else
                    fPaths(i)=0;
                    obs(i,1:length(f1))=f1;
                end
                lPaths(i)=length(P1);
                Paths(1:lPaths(i),i)=P1';
                Pend(i)=P1(end);
            end
            bOK(i)=0;
        end
    end
    iP=find(~bOK&lPaths);
    PCol=zeros(length(iP));
    L=lPaths(iP);
    for i=1:length(iP)
        Pe=Pend(iP(i));
        for j=1:length(iP)
            if i~=j
                lj=L(j);
                PCol(i,j)=any(Paths(1:lj,iP(j))==Pe);
            end
        end
    end
    sPCol=sum(PCol,2);
    pOK=find(sPCol==0);
    pNOK=find(sPCol~=0);
    if isempty(pOK)
        if length(pNOK)==1
            pOK(end+1)=pNOK;
        elseif ~isempty(pNOK)
            if length(pNOK)>1&&any(fPaths(iP(pNOK)))
                pNOK(~fPaths(iP(pNOK)))=[];
            end
            iNOK1=pNOK(find(sPCol(pNOK)==1));
            for i=iNOK1'

                j=find(PCol(i,:));
                jj=iP(j);
                p=iP(i);
                pe=Pend(p);
                A(P(p))=0;
                A(pe)=p;
                [P1,f1]=SearchPath(A,P(jj),Pf(jj));
                A(P(p))=p;
                A(pe)=0;
                if isempty(f1)
                    pOK(end+1)=i;
                    break
                end
            end
        end
    end
    if length(pOK)>1
        obs1=abs(obs(:));
        temp=zeros(1,length(obs1));
        i=1;
        q=0;
        pOK1=pOK;
        while i<=length(obs1)
            temp(obs1==obs1(i))=1;
            if sum(temp)>1
                j=find(obs1==obs1(i));
                obs1(j(2:end))=[];
            end
            if any(obs1(i)==pOK)
                q=1;
                j=find(obs1(i)==pOK);
                pOK1(j)=0;
            end
            i=i+1;
        end
        if q
            pOK(pOK1~=0)=[];
        end
        if length(pOK)>1&&any(fPaths(iP(pOK)))
            pOK(~fPaths(iP(pOK)))=[];
        end
    end
    j=1;
    while j<=length(pOK)
        i=pOK(j);
        b=iP(i);
        k=nmv+1:nmv+L(i);
        mv(k,1)=b;
        mv(k,2)=Paths(1:L(i),b);
        nmv=nmv+L(i);
        A(P(b))=0;
        A(Pend(b))=b;
        P(b)=Pend(b);
        if fPaths(b)
            nNOK=nNOK-1;
        end
        j=j+1;
    end
end
P=Pi;
for i=2:nmv
    b=mv(i);
    p=mv(i,2);
    dp=p-P(b);
    if dp==1
        mv(i,2)=2;
    elseif dp==-1
        mv(i,2)=4;
    elseif dp==m2
        mv(i,2)=1;
    else
        mv(i,2)=3;
    end
    P(b)=p;
end
mv=mv(2:nmv,:);
if nNOK
    rand('state',states);
    mv2=[mv;Faster10IntReps2(A(2:m+1,2:n+1),af,w)];
    score=sum(w(mv2(:,1)));
    if abs(score-optscore)<5;perfectMV=(Dperfect==size(mv2,1));mv=mv2;return;end;
    if abs(score-optscore)>9000;perfectMV=(Dperfect==size(mv2,1));mv=mv2;return;end;%just give up it will never get there
    rand('state',states*2);
    mv1=[mv;Faster10IntReps2(A(2:m+1,2:n+1),af,w)];
    score1=sum(w(mv1(:,1)));
    if score1==score;
        mv=mv2;
        score=score1;
    else
        if score1<score;
            mv2=mv1;
            score=score1;
        end;
        if abs(score-optscore)<15;perfectMV=(Dperfect==size(mv2,1));mv=mv2;return;end;
        rand('state',states*371);
        mv1=[mv;Faster10IntReps2(A(2:m+1,2:n+1),af,w)];
        score1=sum(w(mv1(:,1)));
        if score1==score;mv=mv2;perfectMV=Dperfect==size(mv,1);return;end;
        if score1<score;
            mv2=mv1;score=score1;
        end;
        if abs(score-optscore)<500;perfectMV=(Dperfect==size(mv2,1));mv=mv2;return;end;
        rand('state',states*173);
        mv1=[mv;Faster10IntReps2(A(2:m+1,2:n+1),af,w)];
        score1=sum(w(mv1(:,1)));
        if score1<score;
            mv2=mv1;score=score1;
        end;
        mv=mv2;
    end
    perfectMV=Dperfect==size(mv,1);
else
    score=sum(w(mv(:,1)));
    perfectMV=1;
end
%%%%%%%%%%%%%%%%%%
function [P,stopped]=SearchPath(A,p1,p2)
global Ac Ar m2
c1=Ac(p1);c2=Ac(p2);
r1=Ar(p1);r2=Ar(p2);
stopped=[];
P=[];
if r1>r2
    d_r=-1;
elseif r1<r2
    d_r=1;
else
    d_r=0;
end
if c1>c2
    d_c=-1;
elseif c1<c2
    d_c=1;
else
    d_c=0;
end
n=0;
p=p1;
if d_r==0||d_c==0
    while p~=p2
        np=p+d_c*m2+d_r;
        if A(np)
            stopped=A(np);
            break
        end
        p=np;
        n=n+1;
        P(n)=p;
    end
else
    Ah=A;
    c=c2;
    i1=p2-d_r;
    r_1=r2-d_r;
    while c~=c1
        r=r_1;
        r_1=0;
        i2=i1;
        while r~=r1
            if Ah(i2)
                Afil=-Ah(i2);
                if r==r2
                    r_1=r2;
                    i1=i2-d_c*m2;
                else
                    r_1=r+d_r;
                    i1=i2-d_c*m2+d_r;
                end
                r=r-d_r;
                i2=i2-d_r;
                while r~=r1
                    if Ah(i2)==0
                        Ah(i2)=Afil;
                    end
                    r=r-d_r;
                    i2=i2-d_r;
                end
                if Ah(i2)==0
                    Ah(i2)=Afil;
                end
                break;
            end
            r=r-d_r;
            i2=i2-d_r;
        end
        if r_1==0&&Ah(i2)
            i1=i2-d_c*m2+d_r;
            r_1=r+d_r;
        end
        c=c-d_c;
        if r_1==0
            break;
        end
    end
    r=r2;
    i1=p2-d_c*m2;
    c_1=c2-d_c;
    while r~=r1
        c=c_1;
        c_1=0;
        i2=i1;
        while c~=c1
            if Ah(i2)
                if Ah(i2)<0
                    Afil=Ah(i2);
                else
                    Afil=-Ah(i2);
                end
                if c==c2
                    c_1=c2;
                    i1=i2-d_r;
                else
                    c_1=c+d_c;
                    i1=i2-d_r+d_c*m2;
                end
                c=c-d_c;
                i2=i2-d_c*m2;
                while c~=c1
                    if Ah(i2)==0
                        Ah(i2)=Afil;
                    end
                    c=c-d_c;
                    i2=i2-d_c*m2;
                end
                if Ah(i2)==0
                    Ah(i2)=Afil;
                end
                break;
            end
            c=c-d_c;
            i2=i2-d_c*m2;
        end
        if c_1==0&&Ah(i2)
            i1=i2-d_r+d_c*m2;
            c_1=c+d_c;
        end
        r=r-d_r;
        if c_1==0
            break;
        end
    end
    while p~=p2
        if c1~=c2&&Ah(p+m2*d_c)==0
            di=m2*d_c;
            dc=d_c;
            dr=0;
        elseif r1~=r2&&Ah(p+d_r)==0
            di=d_r;
            dc=0;
            dr=d_r;
        else
            if c1==c2
                stopped=Ah(p+d_r);
            elseif r1==r2
                stopped=Ah(p+m2*d_c);
            else
                stopped=[Ah(p+d_r) Ah(p+m2*d_c)];
            end
            break;
        end
        p=p+di;
        n=n+1;
        P(n)=p;
        r1=r1+dr;
        c1=c1+dc;
    end
end
%%%%%%%%%%%%%%%%%%
function bmovelist=Faster10IntReps2(init,final,wts)
global Dperfect
numtimes=8;
if(max(size(init))>30)&&max(size(wts))/(size(init,1)*size(init,2))>0.25
    numtimes=10;
elseif(max(size(init))>30)
    numtimes=7;
elseif(max(size(init))<11)
    numtimes=2;
end
bscore=1e20;
for repcount=1:numtimes
    [movelist,c]=matrixsolver(init,final,wts);
    bf=length(wts)+1+zeros(size(init)+2);
    bf(2:end-1,2:end-1)=final;
    tries=1;
    maxtries=15;
    while ~isequal(c,bf)&&(tries<maxtries)
        randwts=rand(size(wts));
        [addtomovelist,c]=matrixsolver(c(2:end-1,2:end-1),final,randwts);
        movelist=[movelist;addtomovelist];
        tries=tries+1;
    end
    if ~isequal(c,bf)
        movegoaltries=0;
        while ~isequal(c,bf)&&(movegoaltries<20)
            problemboxes=c(c~=bf & c~=0);
            openspots=find(c==0);
            tempfinal=bf;
            for i=1:length(problemboxes)
                cgind=find(bf==problemboxes(i));
                tempfinal(cgind) =0;
                newind=ceil(rand*length(openspots));
                tempfinal(openspots(newind))=problemboxes(i);
                openspots(newind)=[];
            end
            randwts=rand(size(wts));
            [addtomovelist,c]=matrixsolver(c(2:end-1,2:end-1),tempfinal(2:end-1,2:end-1),randwts);
            movelist=[movelist;addtomovelist];
            movegoaltries=movegoaltries+1;
            aftermovegoaltries=0;
            while ~isequal(c,bf)&&aftermovegoaltries<5
                randwts=rand(size(wts));
                [addtomovelist,c]=matrixsolver(c(2:end-1,2:end-1),final,randwts);
                movelist=[movelist;addtomovelist];
                aftermovegoaltries=aftermovegoaltries+1;
            end
        end
    end
    score=sum(wts(movelist(:,1)));
    if score<bscore
        bmovelist=movelist;
        if length(bmovelist)==Dperfect;return;end;
        bscore=score;
    end
end
%%%%%%%%%%%%%%%%%%
function [movelist,c]=matrixsolver(init,final,wts)
biggermatrix=length(wts)+1+zeros(size(init)+2);
bi=biggermatrix;
bf=bi;
bi(2:end-1,2:end-1)=init;
bf(2:end-1,2:end-1)=final;
c=bi;
[m,n]=size(c);
numboxes=length(wts);
[DNC,wtorder]=sort(wts);
movelist=zeros(0,2);
for i=1:numboxes
    cbn=wtorder(i);
    cr=0;cc=0;fr=0;fc=0;
    for j=2:m-1,
        for k=2:n-1,
            if c(j,k)==cbn,cr=j;cc=k;if fr>0,break;end;end;
            if bf(j,k)==cbn,fr=j;fc=k;if cr>0,break;end;end
        end
        if fr>0&&cr>0,break;end
    end
    dr=fr-cr;
    dc=fc-cc;
    while dr~=0||dc~=0
        neighborhood=[c(cr,cc+1),c(cr+1,cc),c(cr,cc-1),c(cr-1,cc)];
        opendirs=find(~neighborhood);
        desireddirs=[];
        if dr~=0
            desireddirs(end+1)=-sign(dr)+3;
        end
        if dc~=0
            desireddirs(end+1)=-sign(dc)+2;
        end
        pic=opendirs(ismember1(opendirs,desireddirs));
        if ~isempty(pic)
            if length(pic)>1
                if abs(dr)>abs(dc)
                    movedir=desireddirs(1);
                else
                    movedir=desireddirs(2);
                end
            else
                movedir=pic;
            end
        else
            ind=ceil(rand*length(desireddirs));
            boxtomove=neighborhood(desireddirs(ind));
            switch desireddirs(ind)
                case {1,3}
                    rcaxis=-1;
                case {2,4}
                    rcaxis=1;
            end
            [c,movelist,rejectflag]=outoftheway(boxtomove,rcaxis,cbn,c,movelist,bf);
            ootwtries=1;
            while rejectflag&&ootwtries<10
                [c,movelist,rejectflag]=outoftheway(boxtomove,rcaxis,cbn,c,movelist,bf);
                ootwtries=ootwtries+1;
            end
            movedir=desireddirs(ind);
        end
        c(cr,cc)=0;
        switch movedir
            case 1
                cc=cc+1;
            case 2
                cr=cr+1;
            case 3
                cc=cc-1;
            case 4
                cr=cr-1;
        end
        c(cr,cc)=cbn;
        movelist(end+1,:)=[cbn,movedir];
        dr=fr-cr;
        dc=fc-cc;
    end
end
%%%%%%%%%%%%%%%%%%
function [c,movelist,rejectflag]=outoftheway(boxnumber,rcaxis,dontmovetheseboxes,c,movelist,bf)
c_in=c;
movelist_in=movelist;
rejectflag=0;
[cr,cc]=find(c==boxnumber);
neighborhood=[c(cr,cc+1),c(cr+1,cc),c(cr,cc-1),c(cr-1,cc)];
opendirs=find(~neighborhood);
if isempty(opendirs)
    switch rcaxis
        case -1
            bettercandidates=neighborhood([2,4]);
            worsecandidates=neighborhood([1,3]);
        case 1
            bettercandidates=neighborhood([1,3]);
            worsecandidates=neighborhood([2,4]);
    end
    wallnum=c(1,1);
    remainingcandidates=setdiff(bettercandidates,[dontmovetheseboxes,wallnum]);
    if isempty(remainingcandidates)
        remainingcandidates=setdiff(worsecandidates,[dontmovetheseboxes,wallnum]);
        if isempty(remainingcandidates)
            rejectflag=1;
            c=c_in;
            movelist=movelist_in;
            return
        end
    end
    if length(remainingcandidates)>1
        remainingcandidates=remainingcandidates((rand>.515)+1);
    end
    [c,movelist,rejectflag]=outoftheway(remainingcandidates,-rcaxis,[dontmovetheseboxes,boxnumber],c,movelist,bf);
    if rejectflag
        c=c_in;
        movelist=movelist_in;
        return
    end
    movedir=find(neighborhood==remainingcandidates);
else
    [cr,cc]=find(c==boxnumber);
    [fr,fc]=find(bf==boxnumber);
    dr=fr-cr;
    dc=fc-cc;
    desireddirs=[];
    if dr~=0
        desireddirs(end+1)=-sign(dr)+3;
    end
    if dc~=0
        desireddirs(end+1)=-sign(dc)+2;
    end
    pic=opendirs(ismember1(opendirs,desireddirs));
    if ~isempty(pic)
        if length(pic)>1
            if abs(dr)>abs(dc)
                movedir=desireddirs(1);
            else
                movedir=desireddirs(2);
            end
        else
            movedir=pic;
        end
    else
        movedir=opendirs(ceil(rand*length(opendirs)));
    end
end
c(cr,cc)=0;
switch movedir
    case 1
        cc=cc+1;
    case 2
        cr=cr+1;
    case 3
        cc=cc-1;
    case 4
        cr=cr-1;
end
c(cr,cc)=boxnumber;
movelist(end+1,:)=[boxnumber,movedir];
%%%%%%%%%%%%%%%%%%
function [tf]=ismember1(a,s)
numelA=numel(a);
numelS=numel(s);
if numelA==0||numelS<=1
    if (numelA==0||numelS==0)
        tf=false(size(a));
        return
    elseif numelS==1
        tf=(a==s);
        return
    end
else
    tf=false(1,numelA);
    for i=1:numelA;
        tf(i)=any(a(i)==s);
    end;
end
%%%%%%%%%%%%%%%%%%
function [tf]=ismember2(a,s)
numelA=numel(a);
numelS=numel(s);
if numelA==0||numelS<=1
    if (numelA==0||numelS==0)
        tf=false(size(a));
        return
    elseif numelS==1
        tf=(a==s);
        return
    end
else
    tf=false(size(a));
    for i=1:numelA
        found=(a(i)==s(:));
        if ~any(found)
            tf(i)=1;
        end
    end
end
%%%%%%%%%%%%%%%%%%
function c=setdiff(a,b)
c=unique(a(ismember2(a(:),b(:))));
%%%%%%%%%%%%%%%%%%
function b=unique(a)
numelA=numel(a);
if numelA<2
    b=a;
else
    b=sort(a);
    db=diff(b);
    d=db~=0;
    d(numelA)=true;
    b=b(d);
end
%%%%%%%%%%%%%%%%%%
function ndx=sub2ind(siz,varargin)
siz=[siz ones(1,nargin-length(siz)-1)];
mt=cellfun('isempty',varargin);
if any(mt)
    ndx=zeros(~mt);
    return;
end
k=[1 cumprod(siz(1:end-1))];
ndx=1;
for i=1:length(siz),
    v=varargin{i};
    ndx=ndx+(v-1)*k(i);
end
%%%%%%%%%%%%%%%%%%
function p=randperm(n)
[DNC,p]=sort(rand(1,n));
%%%%%%%%%%%%%%%%%%
function P=perms(V)
V=V(:).';
n=length(V);
if n<=1,P=V;
    return;
end
q=perms(1:n-1);
m=size(q,1);
P=zeros(n*m,n);
P(1:m,:)=[n*ones(m,1) q];
for i=n-1:-1:1
    t=q;
    t(t==i)=n;
    P((n-i)*m+1:(n-i+1)*m,:)=[i*ones(m,1) t];
end
P=V(P);
%%%%%%%%%%%%%%%%%%
function mv=itTakesAThief(ai,af,w)
nBlocks=max(ai(:));
[m,n]=size(ai);
ftot=m*n;
steps=zeros(ftot,4);
steps(1,:)=[0 0 m 1];
steps(m,:)=[0 -1 m 0];
steps(ftot-m+1,:)=[-m 0 0 1];
steps(ftot,:)=[-m -1 0 0];
for ci=2:m-1
    steps(ci,:)=[0 -1 m 1];
end
for ci=ftot-m+2:ftot-1
    steps(ci,:)=[-m -1 0 1];
end
for col=m+1:m:ftot-m
    steps(col,:)=[-m 0 m 1];
    steps(col+m-1,:)=[-m -1 m 0];
    for row=1:m-2
        steps(col+row,:)=[-m -1 m 1];
    end
end
I=[0  1  0 -1];
J=[1  0 -1  0];
a=ai;
mv=[];
success=1;
uas=zeros(m,n);
initialmode=1;
while ~isequal(af,a)
    numw=length(w);
    wwb=1:length(w);
    wwbi=a(a==af);
    wwbi(wwbi==0)=[];
    wwb(wwbi)=[];
    [tmp,wwbi]=sort(-w(wwb));
    wwb=wwb(wwbi);
    if (success==0)
      fiter=true;
    else
        fiter=false;
    end
    success=0;
    while (wwb)
        blk=wwb(1);
        wwb(1)=[];
        ci=find(af(:)==blk);
        if (a(ci)>0&&~fiter&&initialmode ~=2)
            continue;
        end
        [smv,fc,a]=findshortestpath(blk,a,af,w,m,n,1,[],steps);
        if (fc==0)
            mv=[mv;smv];
            ci=find(a(:)==blk);
            a(ci)=0;
            ci=find(af(:)==blk);
            a(ci)=blk;
            success=1;
        elseif (initialmode ~=1)
            moves=[m 1 -m -1];
            cpos=1;
            ci=find(a(:)==blk);
            if (initialmode ==0)
                upos=cpos;
                mfb=[];
                ua=uas;
                uci=ci;
                ua(uci)=1;
                while (upos<=size(smv,1))
                    uci=uci+moves(smv(upos,2));
                    upos=upos+1;
                    ua(uci)=1;
                    if (a(uci)>0)
                        mfb=[mfb;a(uci)];
                    end
                end
                [mfok,mfmv,a_mf]=movefurniture(mfb,wwb,a,af,w,m,n,ua,steps);
                if (mfok)
                    mv=[mv;mfmv;smv];
                    a=a_mf;
                    a(ci)=0;
                    ci=find(af(:)==blk);
                    a(ci)=blk;
                    continue;
                end
            end;
            a(ci)=0;
            chmv=[];
            while (cpos<=size(smv,1))
                while (cpos<=size(smv,1)&&a(ci+moves(smv(cpos,2)))==0)
                    if (initialmode)
                        if af(ci+moves(smv(cpos,2)))>0
                            break;
                        end
                    end
                    ci=ci+moves(smv(cpos,2));
                    mv=[mv;smv(cpos,:)];
                    cpos=cpos+1;
                end
                if (initialmode)
                    break;
                end
                if (cpos>size(smv,1))
                    continue;
                end
                upos=cpos;
                ua=uas;
                uci=ci;
                ua(uci)=1;
                while (upos<=size(smv,1))
                    uci=uci+moves(smv(upos,2));
                    upos=upos+1;
                    ua(uci)=1;
                end
                [hmv,fc,a]=findshortestpath(a(ci+moves(smv(cpos,2))),a,af,w,m,n,2,ua,steps);
                if (isempty(hmv))
                    ua=zeros(m,n);
                    ua(ci)=1;
                    ua(ci+moves(smv(cpos,2)))=1;
                    [hmv,fc,a]=findshortestpath(a(ci+moves(smv(cpos,2))),a,af,w,m,n,2,ua,steps);
                end
                mv=[mv;hmv];
                chmv=[chmv;hmv];
                hpos=1;
                while (hpos<=size(hmv,1))
                    hci=find(a(:)==hmv(hpos,1));
                    a(hci)=0;
                    hci=hci+moves(hmv(hpos,2));
                    a(hci)=hmv(hpos,1);
                    hpos=hpos+1;
                end
            end
            a(ci)=blk;
        end
    end
    if (~success&&initialmode==1)
        initialmode=0;
        success=1;
    end
    if (~success&&initialmode==2)
        initialmode=0;
        success=1;
    end
end
%%%%%
function [mfok,mfmv,a_mf,ua]=movefurniture(mfb,wwb,a,af,w,m,n,ua,steps)
fcs=0;
mfmv=[];
mfok=0;
a_mf=a;
moves=[m 1 -m -1];
while (mfb)
    mblk=mfb(1);
    mfb(1)=[];
    mci=find(a_mf(:)==mblk);
    [hmv,fc,a_mf]=findshortestpath(mblk,a_mf,af,w,m,n,2,ua,steps);
    if (isempty(hmv))
        mfok=0;
        return;
    end
    mfmv=[mfmv;hmv];
    hpos=1;
    while (hpos<=size(hmv,1))
        hci=find(a_mf(:)==hmv(hpos,1));
        a_mf(hci)=0;
        hci=hci+moves(hmv(hpos,2));
        a_mf(hci)=hmv(hpos,1);
        ua(hci)=1;

        hpos=hpos+1;
    end
end
while (wwb)
    blk=wwb(1);
    wwb(1)=[];
    ci=find(af(:)==blk);
    if (ua(ci))
        continue;
    end
    [hmv,fc,a_mf]=findshortestpath(blk,a_mf,af,w,m,n,1,[],steps);
    if (fc==0)
        mfmv=[mfmv;hmv];
        ci=find(a_mf(:)==blk);
        a_mf(ci)=0;
        ci=find(af(:)==blk);
        a_mf(ci)=blk;
    end
end
mfok=1;
return;
%%%%%
function [smv,fc,a]=findshortestpath(blk,a,af,w,m,n,mode,ua,steps)
finalmode=false;
smv=[];fc=0;
is=zeros(100,1);
isi=1;
ise=1;
is(1)=find(a(:)==blk);
ca=zeros(m,n);
ca(is(1))=1;
cm=zeros(m,n);
om=zeros(m,n);
fm=zeros(m,n);
helpw=[0;w];
while (isi<=ise)
    ci=is(isi);
    cv=ca(ci);
    t=ci+steps(ci,:);
    if (mode==2)
        t(ua(t)==1)=ci;
    end
    for ind=1:4
        mpenalty=0;
        if (a(t(ind))>0)
            mpenalty=2*helpw(a(t(ind))+1);
        end
        if ca(t(ind))==0
            ca(t(ind))=cv+1;
            if (mode==1)
                cm(t(ind))=cm(ci)+mpenalty+w(blk)+0.1*(a(t(ind))>0);
            else
                cm(t(ind))=cm(ci)+2*helpw(a(t(ind))+1);
            end
            om(t(ind))=om(ci)+(a(t(ind))>0);
            fm(t(ind))=a(ci);
            if (mode==1||a(t(ind))>0)
                ise=ise+1;
                is(ise)=t(ind);
            end
        else
            if (cm(ci)+mpenalty+w(blk)<cm(t(ind)))
                ca(t(ind))=ca(ci)+1;
                cm(t(ind))=cm(ci)+mpenalty+w(blk);
                om(t(ind))=om(ci)+(a(t(ind))>0);
                fm(t(ind))=a(ci);
                if (mode==1||a(t(ind))>0)
                    ise=ise+1;
                    is(ise)=t(ind);
                end
            end
        end
    end
    isi=isi+1;
end
if (mode==1)
    ci=find(af(:)==blk);
else
    cm(ca==0)=Inf;
    mi=[];
    if (finalmode)
        ui=find(a==0 & (af==0 | af==blk));
        minval=min(cm(ui));
        if (~isinf(minval))
            mi=find(cm(ui)==minval);
        end
    end
    if (isempty(mi)||isinf(minval))
        ui=find(a==0);
        minval=min(cm(ui));
        mi=find(cm(ui)==minval);
    end
    if (length(mi)>1)
        minval=1e20;
        for cand=1:length(mi)
            cci=ui(mi(cand));
            cblk=fm(cci);
            row=mod(cci,m);
            col=ceil(cci/m);
            [frow,fcol]=find(af==cblk);
            cblkcost=abs(row-frow)+abs(col-fcol);
            if (cblkcost<minval)
                minval=cblkcost;
                ci=cci;
            end
        end
    else
        ci=ui(mi);
    end
end
cv=ca(ci);
if (cv==0)
    return;
end
tmv=[];
while (cv>1)
    t=ci+steps(ci,:);
    pi=find(ca(t)==cv-1);
    [tmp,ni]=min(cm(t(pi)));
    ci=t(pi(ni));
    if (mode==1)
        tmv(end+1,[1 2])=[blk pi(ni)];
    elseif (a(ci)>0)
        tmv(end+1,[1 2])=[a(ci) pi(ni)];
    end
    cv=cv-1;
end
if (mode==1)
    smv=[tmv(end:-1:1,:)];
    fc=om(find(af(:)==blk));
else
    smv=tmv;
    fc=0;
end
%%%%%
function [bestmv,yess]=dealWall1(ai,af,w,bestmv)
yess=0;aiMap=ai>0;nBlocks=length(w);[m,n]=size(aiMap);
if nBlocks/(m*n)>0.5
    return;
end;
afMap=af>0;mapDif=(ai==af).*aiMap.*afMap;
rsum=sum(mapDif,2);csum=sum(mapDif);
hwall= rsum==n ;vwall= csum==m ;
hwall([1 m])=0;
vwall([1 n])=0;nzh=nnz(hwall);nzv=nnz(vwall);
nzhv=nzh+nzv;
if nzhv~= 1||nzhv~=2
    return;
end;
bestScore=sum(w(bestmv(:,1)));
aiBlock=ai(aiMap);
[aiOrder,aiPos]=sort(aiBlock);
[airow,aicol]=find(aiMap);
airow=airow(aiPos);
aicol=aicol(aiPos);
I=[0  1  0 -1];
J=[1  0 -1  0];
if nzh==1
    hBrickID=ai(hwall,:);
    hwall=find(hwall);
    brickWeight=w(hBrickID);
    [minb,minInd]=sort(brickWeight);
    for i=1:2
        a=ai;
        openDoor=hBrickID(minInd(i));
        di=airow(openDoor);
        dj=aicol(openDoor);
        if hwall<m/2
            ndi=di+I(2);
            ndj=dj+J(2);
            m1=2;
        else
            ndi=di+I(4);
            ndj=dj+J(4);
            m1=4;
        end;
        if a(ndi,ndj)~=0||af(ndi,ndj)~=0
            if hwall<m/2&&hwall>2
                ndi=di+I(4);ndj=dj+J(4);
                m1=4;
            else
                ndi=di+I(2);
                ndj=dj+J(2);
                m1=2;
            end;
            if a(ndi,ndj)~=0||af(ndi,ndj) ~=0
                continue;
            end;
        end;
        if dj<n/2
            ndi=ndi+I(1);
            ndj=ndj+J(1);
            m2=1;
        else
            ndi=ndi+I(3);
            ndj=ndj+J(3);
            m2=3;
        end;
        if a(ndi,ndj)~=0||af(ndi,ndj)~=0
            if dj<n/2&&dj>2
                ndi=ndi+I(3);
                ndj=ndj+J(3);
                m2=3;
            else
                ndi=ndi+I(1);
                ndj=ndj+J(1);
                m2=1;
            end;
            if a(ndi,ndj)~=0||af(ndi,ndj)~=0
                continue;
            end;
        end;
        mvt=[[openDoor,m1];[openDoor,m2]];
        a(di,dj)=0;
        a(ndi,ndj)=openDoor;
        aaf=af;
        aaf(di,dj)=0;
        aaf(ndi,ndj)=openDoor;
        mv1=cbest(a,aaf,w);
        mvt=[mvt;mv1;[openDoor,4-m2];[openDoor,6-m1]];
        curscore=sum(w(mvt(:,1)));
        if curscore<bestScore
            yess=1;
            bestmv=mvt;
            bestScore=curscore;
        end;
    end;
end;
if nzv==1
    vBrickID=ai(:,vwall);
    vwall=find(vwall);
    brickWeight=w(vBrickID);
    [minb,minInd]=sort(brickWeight);
    for i=1:2
        a=ai;
        openDoor=vBrickID(minInd(i));
        di=airow(openDoor);
        dj=aicol(openDoor);
        if vwall<n/2
            ndi=di+I(1);
            ndj=dj+J(1);
            m1=1;
        else
            ndi=di+I(3);
            ndj=dj+J(3);
            m1=3;
        end;
        if a(ndi,ndj)~=0||af(ndi,ndj)~=0
            if vwall<n/2&&vwall>2
                ndi=di+I(3);
                ndj=dj+J(3);
                m1=3;
            else
                ndi=di+I(1);
                ndj=dj+J(1);
                m1=1;
            end;
            if a(ndi,ndj)~=0||af(ndi,ndj) ~=0
                continue;
            end;
        end;
        if di<m/2
            ndi=ndi+I(2);
            ndj=ndj+J(2);
            m2=2;
        else
            ndi=ndi+I(4);
            ndj=ndj+J(4);
            m2=4;
        end;
        if a(ndi,ndj)~=0||af(ndi,ndj)~=0
            if di<m/2&&di>2
                ndi=ndi+I(4);
                ndj=ndj+J(4);
                m2=4;
            else
                ndi=ndi+I(2);
                ndj=ndj+J(2);
                m2=2;
            end;
            if a(ndi,ndj)~=0||af(ndi,ndj)~=0
                continue;
            end;
        end;
        mvt=[[openDoor,m1];[openDoor,m2]];
        a(di,dj)=0;
        a(ndi,ndj)=openDoor;
        aaf=af;
        aaf(di,dj)=0;
        aaf(ndi,ndj)=openDoor;
        mv1=cbest(a,aaf,w);
        mvt=[mvt;mv1;[openDoor,6-m2];[openDoor,4-m1]];
        curscore=sum(w(mvt(:,1)));
        if curscore<bestScore
            yess=1;
            bestmv=mvt;
            bestScore=curscore;
        end;
    end;
end;
mv=bestmv;
