require('./index.html');
require('./main.sass');
import * as firebase from 'firebase';


// Elm
const { Elm } = require("./Main.elm")
const elmnode = document.getElementById('elmapp')
const app     = Elm.Main.init( { node:elmnode
                               }
                             )
//document.getElementByClassName("chatlog_o").contentWindow.scrollBy(0,100);
//投稿のたびにスクロールしたい


// Firebase
const get_fbconf = require('./firebase_config.js')
firebase.initializeApp(get_fbconf())
const provider = new firebase.auth.GoogleAuthProvider()



// Elm -> Firebase
app.ports.signIn.subscribe( _ => {
    firebase.auth().signInWithPopup(provider).then((_) => {}).catch((error) => {})
})

app.ports.signOut.subscribe( _ => {
    firebase.auth().signOut().then(() => { console.log("Signed out.") })
})



// Firebase -> Elm
//setTimeout( () => app.ports.rcvState.send("Done!") , 3000)

firebase.auth().onAuthStateChanged( (u) => {
    if(u){
        app.ports.checkSign.send( u.email )
    } else {
        app.ports.checkSign.send( null )
    }
})

