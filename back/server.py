# -*- coding: utf-8 -*-
from flask import Flask
from flask import render_template
from flask import make_response, jsonify, abort
from flask_cors import CORS
import time

app = Flask(__name__)
CORS(app)


@app.errorhandler(404)
def not_found(error):
    return make_response(jsonify({'error': 'Not found'}), 404)


@app.route('/')
def index():
    return render_template("index.html")


@app.route('/rvs_str/<string:msg>', methods=['GET'])
def reverse_str(msg):
    rev = msg[::-1]
    time.sleep(5)
    return make_response(rev)
    # return make_response(json.dumps(result, ensure_ascii=False))


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=3030)

