import React from 'react'
import { test } from './Test.purs'
import logo from './logo.svg'
import './App.css'

const App = function (props, context) {
  console.log(props)
  console.log(context)
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
    </div>
  )
}

export default App
