import React from 'react'
import { test, halogenApp } from './Test.purs'
import { queryCollectibles } from './GraphQLTest.purs'
import logo from './logo.svg'
import './App.css'

const logIt = () => {
  console.log('koook')
}

const App = function (props, context) {
  console.log(props)
  console.log(context)
  halogenApp().then(async ({ clickButton, clickEvent }) => {
    await clickButton()
    clickEvent(logIt)()
  })
  queryCollectibles()
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          {test('from Purescript, ')}
          <code>src/App.js</code>
          {' '}
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
      <div id="halogenApp" />
    </div>
  )
}

export default App
