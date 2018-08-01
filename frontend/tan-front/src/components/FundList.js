import React, { Component } from 'react';


class FundList extends Component {
  
  constructor() {
      super();
      this.state = {
          funds : [],
      };
  }

  componentDidMount() {
    fetch('http://localhost:8080/funds')
    .then(results => {
        console.log(results.json());
     //   return results.json();
    })
  }

  render() {
    return (
      <div className="FundList">
        <p> FundList </p>
      </div>
    );
  }
}

export default FundList;
