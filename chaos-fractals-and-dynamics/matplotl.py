import casioplot as plt

limits=[40,165,384,0]
fenetre=[0,1,0,1]
points=[[]]
lines=[[]]
textes=[[]]
xmin,xmax,ymin,ymax=0,1,0,1
win_scaling='init'
axis_display='on'
color_auto=['b','r','g','k','m','c','y']
color_count=0

def axis(*L):
    global fenetre,win_scaling,axis_display
    if L==():
        if win_scaling=='auto':
            if xmin==xmax:
                if xmin==0:
                    fenetre[0:2]=[-0.05,0.05]
                else:
                    fenetre[0:2]=[0.95*xmin,1.05*xmin]
            else:
                fenetre[0:2]=[xmin-0.05*(xmax-xmin),xmax+0.05*(xmax-xmin)]
            if ymin==ymax:
                if ymin==0:
                    fenetre[2:4]=[-0.05,0.05]
                else:
                    fenetre[2:4]=[0.95*ymin,1.05*ymin]
            else:
                fenetre[2:4]=[ymin-0.05*(ymax-ymin),ymax+0.05*(ymax-ymin)]
        return fenetre
    elif isinstance(L[0],(list,tuple)) and len(L[0])==4:
        fenetre=list(L[0])
        if fenetre[0]==fenetre[1]:
            if fenetre[0]==0:
                fenetre[0:2]=[-0.05,0.05]
            else:
                fenetre[0:2]=[0.95*fenetre[0],1.05*fenetre[0]]
            raise Exception('Userwarning: attempting to set identical bottom == top in function axis(); automatically expanding.')
        if fenetre[2]==fenetre[3]:
            if fenetre[2]==0:
                fenetre[2:4]=[-0.05,0.05]
            else:
                fenetre[2:4]=[0.95*fenetre[2],1.05*fenetre[2]]
            raise Exception('Userwarning: attempting to set identical bottom == top in function axis(); automatically expanding.')
        win_scaling='fixed'
        axis_display='on'
        return fenetre
    elif L[0]=='off':
        axis_display='off'
    elif L[0]=='on':
        axis_display='on'
    elif L[0]=='auto':
        win_scaling='auto'
        if xmin==xmax:
            if xmin==0:
                fenetre[0:2]=[-0.05,0.05]
            else:
                fenetre[0:2]=[0.95*xmin,1.05*xmin]
        else:
            fenetre[0:2]=[xmin-0.05*(xmax-xmin),xmax+0.05*(xmax-xmin)]
        if ymin==ymax:
            if ymin==0:
                fenetre[2:4]=[-0.05,0.05]
            else:
                fenetre[2:4]=[0.95*ymin,1.05*ymin]
        else:
            fenetre[2:4]=[ymin-0.05*(ymax-ymin),ymax+0.05*(ymax-ymin)]
        return fenetre
    else:
        raise Exception('function axis() : error using arguments')

def text(x,y,txt):
    global textes
    if textes==[[]]:
        textes[0]=[x,y,txt]
    else:
        if [x,y,txt] not in textes :
            textes+=[[x,y,txt]]

def plot(*L,**kwargs):
    global color_count,win_scaling
    # if len(L)==2:
    #     L=([list(L[0]),list(L[1])])
    def plotpoint(x,y,c):
        global points,xmin,xmax,ymin,ymax
        if points==[[]]:
            points[0]=[x,y,c]
            xmin=xmax=x
            ymin=ymax=y
        else:
            if [x,y,c] not in points :
                points+=[[x,y,c]]
                xmin=min(x,xmin)
                xmax=max(x,xmax)
                ymin=min(y,ymin)
                ymax=max(y,ymax)

    def plotline(x1,y1,x2,y2,c):
        global lines,xmin,xmax,ymin,ymax
        if lines==[[]]:
            lines[0]=[x1,y1,x2,y2,c]
            xmin=min(x1,x2)
            xmax=max(x1,x2)
            ymin=min(y1,y2)
            ymax=max(y1,y2)
        else:
            if [x1,y1,x2,y2,c] not in lines :
                lines+=[[x1,y1,x2,y2,c]]
                xmin=min(x1,x2,xmin)
                xmax=max(x1,x2,xmax)
                ymin=min(y1,y2,ymin)
                ymax=max(y1,y2,ymax)

    color=kwargs.get('color',None)
    if color!=None and not color in ['b','r','g','k','m','c','y','w','blue','red','green','black','magenta','cyan','yellow','white']:
        raise ValueError('function plot() : unknown color code')
    if len(L)==2 and isinstance(L[0],(int,float)) and isinstance(L[1],(int,float)):
        plotpoint(L[0],L[1],color)
        if win_scaling=='init':
            win_scaling='auto'
    elif len(L)==2 and isinstance(L[0],(list,tuple)) and isinstance(L[1],(list,tuple)):
        if (len(L[0])==len(L[1])):
            if color==None:
                color=color_auto[color_count%7]
                color_count+=1
            for i in range(len(L[0])-1):
                plotline(L[0][i],L[1][i],L[0][i+1],L[1][i+1],color)
            if win_scaling=='init':
                win_scaling='auto'
        else:
            raise ValueError('function plot() : x and y must have same dimension')
    elif len(L)==1 and isinstance(L[0],(list,tuple)):
        if color==None:
            color=color_auto[color_count%7]
            color_count+=1
        for i in range(len(L[0])-1):
            plotline(i,L[0][i],i+1,L[0][i+1],color)
        if win_scaling=='init':
            win_scaling='auto'
    elif len(L)==3 and isinstance(L[0],(int,float)) and isinstance(L[1],(int,float)) and isinstance(L[2],(str)):
        color=L[2]
        if (len(color)==2 and color[0] in ['b','r','g','k','m','c','y','w']) and color[1] in ['o','.','+','*','-']:
            plotpoint(L[0],L[1],color[0])
            if win_scaling=='init':
                win_scaling='auto'
        elif color in ['b','r','g','k','m','c','y','w','blue','red','green','black','magenta','cyan','yellow','white']:
            plotpoint(L[0],L[1],color)
            if win_scaling=='init':
                win_scaling='auto'
        elif color in ['o','.','+','*','-']:
            color=color_auto[color_count%7]
            color_count+=1
            plotpoint(L[0],L[1],color)
            if win_scaling=='init':
                win_scaling='auto'
        else:
            raise ValueError('function plot() : available color codes are b,r,g,k,m,c,y,w')
    elif len(L)==3 and isinstance(L[0],(list,tuple)) and isinstance(L[1],(list,tuple)) and isinstance(L[2],(str)):
        if (len(L[0])==len(L[1])):
            color=L[2]
            if (len(color)==2 and color[0] in ['b','r','g','k','m','c','y','w']) and color[1] in ['o','.','+','*','-']:
                for i in range(len(L[0])-1):
                    plotline(L[0][i],L[1][i],L[0][i+1],L[1][i+1],color[0])
                if win_scaling=='init':
                    win_scaling='auto'
            elif color in ['b','r','g','k','m','c','y','w','blue','red','green','black','magenta','cyan','yellow','white']:
                for i in range(len(L[0])-1):
                    plotline(L[0][i],L[1][i],L[0][i+1],L[1][i+1],color)
                if win_scaling=='init':
                    win_scaling='auto'
            elif color in ['o','.','+','*','-']:
                color=color_auto[color_count%7]
                color_count+=1
                for i in range(len(L[0])-1):
                    plotline(L[0][i],L[1][i],L[0][i+1],L[1][i+1],color)
                if win_scaling=='init':
                    win_scaling='auto'
            else:
                raise ValueError('function plot() : available color codes are b,r,g,k,m,c,y,w')
        else:
            raise ValueError('function plot() : x and y must have same dimension')
    elif len(L)==2 and isinstance(L[0],(list,tuple)) and isinstance(L[1],(str)):
        color=L[1]
        if (len(color)==2 and color[0] in ['b','r','g','k','m','c','y','w']) and color[1] in ['o','.','+','*','-']:
            for i in range(len(L[0])-1):
                plotline(i,L[0][i],i+1,L[0][i+1],color[0])
            if win_scaling=='init':
                win_scaling='auto'
        elif color in ['b','r','g','k','m','c','y','w','blue','red','green','black','magenta','cyan','yellow','white']:
            for i in range(len(L[0])-1):
                plotline(i,L[0][i],i+1,L[0][i+1],color)
            if win_scaling=='init':
                win_scaling='auto'
        elif color in ['o','.','+','*','-']:
            color=color_auto[color_count%7]
            color_count+=1
            for i in range(len(L[0])-1):
                plotline(i,L[0][i],i+1,L[0][i+1],color)
            if win_scaling=='init':
                win_scaling='auto'
        else:
            raise ValueError('function plot() : available color codes are b,r,g,k,m,c,y,w')
    else:
        raise Exception('function plot() : error using arguments')

def show():
    global fenetre, limits, points, lines, textes, xmin, ymin, xmax, ymax, win_scaling, axis_display, color_count

    def RGB(c):
        if c=="k" or c=="black":
            return (0,0,0)
        elif c=="b" or c=="blue":
            return (0,0,255)
        elif c=="g" or c=="green":
            return (0,255,0)
        elif c=="r" or c=="red":
            return (255,0,0)
        elif c=="c" or c=="cyan":
            return (0,255,255)
        elif c=="y" or c=="yellow":
            return (255,255,0)
        elif c=="m" or c=="magenta":
            return (255,0,255)
        elif c=="w" or c=="white":
            return (255,255,255)
        else:
            raise ValueError("invalid color code")

    def printable(x,y):
        global limits
        return(limits[0]<=x<=limits[2] and limits[3]<=y<=limits[1])

    def echelle(a,b):
        k=0
        e=abs(b-a)
        while e>=10 :
            e/=10
            k+=1
        while e<1 :
            e*=10
            k-=1
        return k

    def pas(a,b):
        pas=10**echelle(a,b)
        while (abs(b-a))//pas<4:
            pas/=2
        return pas

    def converttopixel(x,y):
        global fenetre,limits
        ax=(limits[2]-limits[0])/(fenetre[1]-fenetre[0])
        bx=limits[0]-ax*fenetre[0]
        xpixel=round(ax*x+bx)
        ay=(limits[3]-limits[1])/(fenetre[3]-fenetre[2])
        by=limits[1]-ay*fenetre[2]
        ypixel=round(ay*y+by)
        return xpixel,ypixel

    color_count=0
    plt.clear_screen()
    if win_scaling=='auto':
        if xmin==xmax:
            if xmin==0:
                fenetre[0:2]=[-0.05,0.05]
            else:
                fenetre[0:2]=[0.95*xmin,1.05*xmin]
        else:
            fenetre[0:2]=[xmin-0.05*(xmax-xmin),xmax+0.05*(xmax-xmin)]
        if ymin==ymax:
            if ymin==0:
                fenetre[2:4]=[-0.05,0.05]
            else:
                fenetre[2:4]=[0.95*ymin,1.05*ymin]
        else:
            fenetre[2:4]=[ymin-0.05*(ymax-ymin),ymax+0.05*(ymax-ymin)]
    if axis_display=='on' or axis_display=='boxplot':
        for i in range(limits[0],limits[2]+1):
            plt.set_pixel(i,limits[1],RGB("k"))
        for j in range(limits[3],limits[1]+1):
            plt.set_pixel(limits[0],j,RGB("k"))
        fenetreb=sorted([fenetre[0],fenetre[1]])+sorted([fenetre[2],fenetre[3]])
        gx=round(fenetreb[0],-echelle(fenetreb[0],fenetreb[1]))
        gy=round(fenetreb[2],-echelle(fenetreb[2],fenetreb[3]))
        if axis_display=='boxplot':
            for i in range(nbre_boite):
                y=fenetre[2]
                xpixel,ypixel=converttopixel(i+1,y)
                plt.set_pixel(xpixel,ypixel+1,RGB("k"))
                plt.set_pixel(xpixel,ypixel+2,RGB("k"))
                plt.set_pixel(xpixel,ypixel+3,RGB("k"))
                plt.draw_string(xpixel,ypixel+13,str(round(i+1,8)),[0,0,0],"small")
        else :
            for i in range(-11,11):
                x=gx+i*pas(fenetreb[0],fenetreb[1])
                y=fenetre[2]
                xpixel,ypixel=converttopixel(x,y)
                if printable(xpixel,ypixel):
                    plt.set_pixel(xpixel,ypixel+1,RGB("k"))
                    plt.set_pixel(xpixel,ypixel+2,RGB("k"))
                    plt.set_pixel(xpixel,ypixel+3,RGB("k"))
                    plt.draw_string(xpixel,ypixel+13,str(round(x,8)),[0,0,0],"small")
        for j in range(-11,11):
            x=fenetre[0]
            y=gy+j*pas(fenetreb[2],fenetreb[3])
            xpixel,ypixel=converttopixel(x,y)
            if printable(xpixel,ypixel):
                plt.set_pixel(xpixel-1,ypixel,RGB("k"))
                plt.set_pixel(xpixel-2,ypixel,RGB("k"))
                plt.set_pixel(xpixel-3,ypixel,RGB("k"))
                plt.draw_string(xpixel-40,ypixel,str(round(y,8)),[0,0,0],"small")
    if points!=[[]]:
        if points[0]==[]:
            del points[0]
        for i in range(len(points)):
            xpixel,ypixel=converttopixel(points[i][0],points[i][1])
            if printable(xpixel,ypixel) and points[i][2]!=None:
                for j in range(-2,3):
                    plt.set_pixel(xpixel+j,ypixel,RGB(points[i][2]))
                    plt.set_pixel(xpixel,ypixel+j,RGB(points[i][2]))
    if textes!=[[]]:
        if textes[0]==[]:
            del textes[0]
        for i in range(len(textes)):
            xpixel,ypixel=converttopixel(textes[i][0],textes[i][1])
            if printable(xpixel,ypixel):
                plt.draw_string(xpixel,ypixel,textes[i][2],[0,0,0],"small")
    if lines!=[[]]:
        if lines[0]==[]:
            del lines[0]
        for i in range(len(lines)):
            xpixel1,ypixel1=converttopixel(lines[i][0],lines[i][1])
            xpixel2,ypixel2=converttopixel(lines[i][2],lines[i][3])
            deltax=abs(xpixel2-xpixel1)
            deltay=abs(ypixel2-ypixel1)
            if deltax==deltay==0:
                if printable(xpixel1,ypixel1):
                    plt.set_pixel(xpixel1,ypixel1,RGB(lines[i][4]))
            if deltax<=1 and deltay<=1:
                if printable(xpixel1,ypixel1):
                    plt.set_pixel(xpixel1,ypixel1,RGB(lines[i][4]))
                    plt.set_pixel(xpixel2,ypixel2,RGB(lines[i][4]))
            if deltax>=deltay and deltax!=0:
                m=(ypixel2-ypixel1)/(xpixel2-xpixel1)
                p=ypixel1-m*xpixel1
                xpixelmin=max(limits[0],min(xpixel1,xpixel2))
                xpixelmax=min(limits[2],max(xpixel1,xpixel2))
                if xpixelmin<=limits[2] and xpixelmax>=limits[0]:
                    for xpixel in range(xpixelmin,xpixelmax+1):
                        ypixel=round(m*xpixel+p)
                        if printable(xpixel,ypixel):
                            plt.set_pixel(xpixel,ypixel,RGB(lines[i][4]))
            if deltay>deltax:
                m=(xpixel2-xpixel1)/(ypixel2-ypixel1)
                p=xpixel1-m*ypixel1
                ypixelmin=max(limits[3],min(ypixel1,ypixel2))
                ypixelmax=min(limits[1],max(ypixel1,ypixel2))
                if ypixelmin<=limits[1] and ypixelmax>=limits[3]:
                    for ypixel in range(ypixelmin,ypixelmax+1):
                        xpixel=round(m*ypixel+p)
                        if printable(xpixel,ypixel):
                            plt.set_pixel(xpixel,ypixel,RGB(lines[i][4]))
    axis([limits[0]-50,limits[2],limits[1]+50,limits[3]])
    axis("off")
    plt.show_screen()
    points=[[]]
    lines=[[]]
    textes=[[]]
    xmin,xmax,ymin,ymax=0,1,0,1
    fenetre=[0,1,0,1]
    axis_display='on'
    win_scaling='init'
    color_count=0

def bar(val,eff,lar=0.8):
    val=list(val)
    eff=list(eff)
    global color_count
    if isinstance(val,(list,tuple)) and isinstance(eff,(list,tuple)):
        if len(val)==len(eff):
            for i in range(len(val)):
                plot([val[i]-lar/2,val[i]-lar/2],[0,eff[i]],color_auto[color_count%7])
                plot([val[i]+lar/2,val[i]+lar/2],[0,eff[i]],color_auto[color_count%7])
                plot([val[i]-lar/2,val[i]+lar/2],[eff[i],eff[i]],color_auto[color_count%7])
            color_count+=1
        else:
            raise ValueError('function bar() : lists must have same dimension')
    elif isinstance(val,(int,float)) and isinstance(eff,(int,float)):
        for i in range(len(val)):
            plot([val[i]-lar/2,val[i]-lar/2],[0,eff[i]],color_auto[color_count%7])
            plot([val[i]+lar/2,val[i]+lar/2],[0,eff[i]],color_auto[color_count%7])
            plot([val[i]-lar/2,val[i]+lar/2],[eff[i],eff[i]],color_auto[color_count%7])
        color_count+=1
    else:
        raise ValueError('function bar() : error using arguments or arguments not available in this version')

def scatter(xlist,ylist):
    xlist=list(xlist)
    ylist=list(ylist)
    global color_count
    if isinstance(xlist,(list,tuple)) and isinstance(ylist,(list,tuple)):
        if len(xlist)==len(ylist):
            for i in range(len(xlist)):
                plot(xlist[i],ylist[i],color_auto[color_count%7])
            color_count+=1
        else:
            raise ValueError('function scatter() : x and y must have same dimension')

    elif isinstance(xlist,(int,float)) and isinstance(ylist,(int,float)):
        plot(xlist,ylist,color_auto[color_count%7])
        color_count+=1
    else:
        raise ValueError('function scatter() : error using arguments or arguments not available in this version')

def boxplotFR(L):
    L=list(L)
    global fenetre,color_count,nbre_boite, axis_display,win_scaling
    print("boxplotFR:definition \nfrancaise du \ndiagramme en boite")
    axis_display='boxplot'
    n=len(L)
    if type(L[0])==int or type(L[0])==float:
        n=1
    nbre_boite=n
    largeur=0.3/n

    def mediane(l):
      l=sorted(l)
      r=len(l)
      if r%2==0:
         return (l[r//2]+l[r//2-1])/2
      else:
         return l[r//2]

    def quartiles(l):
      l=sorted(l)
      r=len(l)
      return (l[r//4],l[(3*r)//4])

    def deciles(l):
      l=sorted(l)
      r=len(l)
      return (l[r//10],l[(9*r)//10])

    for i in range(n):
        if n==1:
            if type(L[0])==int or type(L[0])==float:
              K=L
            else:
                K=L[0]
        else :
            K=L[i]
        if type(K)==int or type(K)==float:
            plot([i+1-largeur,i+1+largeur],[K,K],'r')
        elif type(K[0])==int or type(K[0])==float:
            K=sorted(K)
            p=len(K)
            Q1=quartiles(K)[0]
            Q3=quartiles(K)[1]
            D1=deciles(K)[0]
            D9=deciles(K)[1]
            plot([i+1-largeur,i+1+largeur,i+1+largeur,i+1-largeur,i+1-largeur],[Q1,Q1,Q3,Q3,Q1],'k')
            plot([i+1,i+1],[Q1,D1],'k')
            plot([i+1,i+1],[Q3,D9],'k')
            plot([i+1-largeur/2,i+1+largeur/2],[D1,D1],'k')
            plot([i+1-largeur/2,i+1+largeur/2],[D9,D9],'k')
            plot(i+1,K[0],'k')
            plot(i+1,K[p-1],'k')
            plot([i+1-largeur,i+1+largeur],[mediane(K),mediane(K)],'r')
        elif type(min(L[0]))!=int and type(min(L[0]))!=float:
            raise ValueError('wrong type of argument')
    if type(L[0])==int or type(L[0])==float:
        fenetre=[0,2,min(L)-1,max(L)+1]
        Max=max(L)
    else:
        Min=min(L[0])
        Max=max(L[0])
        for i in range(len(L)):
            if type(L[i])==int or type(L[i])==float:
                if L[i]<Min:
                    Min=L[i]
                if L[i]>Max:
                    Max=L[i]
            else:
                if min(L[i])<Min:
                  Min=min(L[i])
                if max(L[i])>Max:
                  Max=max(L[i])
        fenetre=[0,len(L)+1,Min-1,Max+1]
    win_scaling='fixed'
    text(len(L)+1/4,Max+1/2,"boxplotFR")

def boxplot(L,**kwargs):
    L=list(L)
    global fenetre,color_count,nbre_boite, axis_display,win_scaling
    whis=kwargs.get('whis',1.5)
    axis_display='boxplot'
    n=len(L)
    if type(L[0])==int or type(L[0])==float:
        n=1
    nbre_boite=n
    largeur=0.3/n
    def mediane(l):
        r=1
        if type(l)!=int and type(l)!=float:
            l=sorted(l)
            r=len(l)
            if r%2==0 and r//2>0:
                return (l[r//2]+l[r//2-1])/2,l[:r//2],l[r//2:]
            else:
                return l[r//2],l[:r//2],l[r//2+1:]
        return l,l,l
    if type(L[0])==int or type(L[0])==float:
        if  min(L)==max(L):
            ampl=1
        else:
            ampl=max(L)-min(L)
        fenetre=[0,2,min(L)-ampl/20,max(L)+ampl/20]
    else:
        Min=min(L[0])
        Max=max(L[0])
        for i in range(len(L)):
            if type(L[i])==int or type(L[i])==float:
                if L[i]<Min:
                    Min=L[i]
                if L[i]>Max:
                    Max=L[i]
            else:
                if min(L[i])<Min:
                  Min=min(L[i])
                if max(L[i])>Max:
                  Max=max(L[i])
        if  Min==Max:
            ampl=1
        else:
            ampl=Max-Min
        fenetre=[0,len(L)+1,Min-ampl/20,Max+ampl/20]
    win_scaling='fixed'
    for i in range(n):
        if n==1:
            if type(L[0])==int or type(L[0])==float:
              K=L
            else:
                K=L[0]
        else :
            K=L[i]
        if type(K)==int or type(K)==float:
            plot([i+1-largeur,i+1+largeur],[K,K],'r')
        elif type(K[0])==int or type(K[0])==float:
            K=sorted(K)
            p=len(K)
            Q1,Q3=mediane(mediane(K)[1])[0],mediane(mediane(K)[2])[0]
            down=0
            if Q1-whis*(Q3-Q1)<=K[0]:
                down=0
            else :
                while Q1-whis*(Q3-Q1)>K[down]:
                    down+=1
            up=p-1
            if Q3+whis*(Q3-Q1)>=K[p-1]:
                up=p-1
            else :
                while Q3+whis*(Q3-Q1)<K[up]:
                    up-=1
            plot([i+1-largeur,i+1+largeur,i+1+largeur,i+1-largeur,i+1-largeur],[Q1,Q1,Q3,Q3,Q1],'k')
            plot([i+1,i+1],[Q1,K[down]],'k')
            plot([i+1,i+1],[Q3,K[up]],'k')
            plot([i+1-largeur/2,i+1+largeur/2],[K[down],K[down]],'k')
            plot([i+1-largeur/2,i+1+largeur/2],[K[up],K[up]],'k')
            from math import pi
            from math import cos
            from math import sin
            if down>0:
             for t in range(down):
                x=[i+1+0.05*(fenetre[1])/3*cos(2*j*pi/50) for j in range(50)]
                y=[K[t]+0.05*(fenetre[3]-fenetre[2])/3*sin(2*j*pi/50) for j in range(50)]
                plot(x,y,'k')
            if up<p-1:
                for t in range(p-1-up):
                    x=[i+1+0.05*(fenetre[1])/3*cos(2*j*pi/50) for j in range(50)]
                    y=[K[p-1-t]+0.05*(fenetre[3]-fenetre[2])/3*sin(2*j*pi/50) for j in range(50)]
                    plot(x,y,'k')
            plot([i+1-largeur,i+1+largeur],[mediane(K)[0],mediane(K)[0]],'r')
        elif type(min(L[0]))!=int and type(min(L[0]))!=float:
            raise ValueError('wrong type of argument')

def arrow(x,y,dx,dy,**kwargs):
    global win_scaling
    a=x+dx
    b=y+dy
    win_scaling='fixed'
    color=kwargs.get('ec','k')
    color=kwargs.get('edgecolor',color)
    headcolor=kwargs.get('fc',color)
    headcolor=kwargs.get('facecolor',headcolor)
    L=kwargs.get('head_width',0.003)
    l=kwargs.get('head_length',1.5*L)
    plot((x,a),(y,b),color)

    def resol(A,B,C):
        D=B**2-4*A*C
        if D>0:
            return((-B-D**0.5)/(2*A),(-B+D**0.5)/(2*A))
    if dx==0:
        if dy>=0:
            c=1
        else:
            c=-1
        plot((a-L/2,a+L/2,a,a-L/2),(b,b,b+c*l,b),color)
    elif dy==0:
        if dx>=0:
            c=1
        else:
            c=-1
        plot((a,a,a+c*l,a),(b-L/2,b+L/2,b,b-L/2),color)
    else:
        m=dy/dx
        p=y-m*x
        S=resol(m**2+1,2*(-a-b*m+m*p),p**2+a**2+b**2-l**2-2*b*p)
        if S[0]*dx<0:
            X=S[0]
        else:
            X=S[1]
        Y=m*X+p
        k=b+a/m
        T=resol(1+1/m**2,2*(-a-k/m+b/m),a**2+k**2-2*b*k+b**2-(L**2)/4)
        plot((T[0],T[1],X,T[0]),(-T[0]/m+k,-T[1]/m+k,Y,-T[0]/m+k),headcolor)
