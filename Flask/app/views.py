from flask import render_template, request
from app import app
import pymysql as mdb


@app.route('/')
@app.route('/index')
def index():
	return render_template("index.html",
        title = 'Home', user = { 'nickname': 'Miguel' },
        )

@app.route('/db')
def cities_page():
    db = mdb.connect(user="root", host="localhost", db="food_db",  charset='utf8')

    with db: 
		cur = db.cursor()
		cur.execute("SELECT name FROM food_tb LIMIT 15;")
		query_results = cur.fetchall()
		
    restaurants = ""
    
    for result in query_results:
		restaurants += result[0]
		restaurants += "<br>"
    return restaurants
    
@app.route("/db_fancy")
def cities_page_fancy():
    db = mdb.connect(user="root", host="localhost", db="food_db",  charset='utf8')
	
    with db:
		cur = db.cursor()
		cur.execute("SELECT name, addresses, mean_satfat \
		FROM food_tb \
		ORDER BY mean_satfat \
		LIMIT 15;")
		query_results = cur.fetchall()
		
    restaurants = []
	
    for result in query_results:
		restaurants.append(dict(name=result[0], address=result[1], price=result[2], score=result[3]))
	
    return render_template('restaurants.html', restaurants = restaurants)

@app.route('/input')
def cities_input():
  return render_template("input.html")

@app.route('/output')
def cities_output():
  #pull 'ID' from input field and store it
  price = request.args.get('ID')
  
  db = mdb.connect(user="root", host="localhost", db="food_db", charset='utf8')

  with db:
    cur = db.cursor()
    #just select the city from the world_innodb that the user inputs
    cur.execute("SELECT name, addresses, price, health_score \
    FROM food_tb \
    WHERE price = '%s' \
    ORDER BY health_score DESC \
    LIMIT 15;" % price)

    query_results = cur.fetchall()

  restaurants = []
  for result in query_results:
    restaurants.append(dict(name=result[0], address=result[1], price=result[2], score=result[3]))
  the_result = ''
  return render_template("output.html", restaurants = restaurants, the_result = the_result)
