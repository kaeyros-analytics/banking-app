import random
import datetime
import math
import json
import pandas as pd

# Nombre de comptes
NB_COMPTES = 1000
# Nombre de comptes dormants
NB_COMPTES_DORMANTS = 200
# Durée de la simulation (en années)
DUREE_SIMULATION = 5
# Devise utilisée
DEVISE = "XAF"
AGIOS_AMOUNT = 7500


# Génération d'une date aléatoire
def generer_date(date_debut, date_fin):
    return date_debut + datetime.timedelta(days=random.randint(0, (date_fin - date_debut).days))

# Génération d'un montant aléatoire
def generer_montant():
    return round(random.uniform(15000, 10000000), 2)

# Génération d'un type de transaction aléatoire
def generer_type_transaction():
    return random.choice(["retrait", "virement"])

# Génération d'une ville aléatoire
def generer_ville():
    return random.choice(["Yaoundé", "Douala", "Bamenda", "Maroua", "Garoua"])

# Génération d'une agence aléatoire
def generer_agence():
    return random.choice(["Agence Centrale", "Agence Akwa", "Agence Bonamoussadi", "Agence Douala", "Agence Garoua"])

def find_end_of_months(start_date, end_date):
    date_range = pd.date_range(start=start_date, end=end_date, freq='MS')  # 'MS' stands for Month Start
    end_of_months = date_range.to_period('M').to_timestamp('M').to_list()
    return end_of_months


# Génération d'un compte
def generer_compte():
    date_debut = datetime.datetime(2015, 1, 1)
    date_fin = datetime.datetime(2020, 1, 1)
    date_fin_creation = datetime.datetime(2023, 1, 1)
    creation_date = generer_date(date_debut, date_fin)
    closing_date = generer_date(creation_date, date_fin_creation)
    return {
        "account_id": random.randint(1, NB_COMPTES),
        "creation_date": creation_date.isoformat(),
        "closing_date": closing_date.isoformat() if random.randint(0, 20) == 20 else None,
        "first_name": random.choice(["Jean", "Marie", "Pierre", "Paule", "Jacques"]),
        "last_name": random.choice(["Dupont", "Martin", "Bernard", "Robert", "Leroy"]),
        "dateofbirth": datetime.datetime.strptime("1970-01-01", "%Y-%m-%d").isoformat(),
        "citizenship": random.choice(["Camerounais", "Camerounais", "Camerounais", "Camerounais", "Camerounais", "Français", "Sénégalais", "Gabonais", "Togolais"]),
        "location": random.choice(["Yaoundé", "Douala", "Bamenda", "Maroua", "Garoua"]),
        "geo_location": (random.uniform(-11.7, 11.5), random.uniform(3.2, 14.7)),
        "occupation": random.choice(["Employé", "Commerçant", "Profession libérale", "Agriculteur", "Sans emploi"]),
        "average_monthly_revenue": random.randint(100000, 1000000),
        "marital_status": random.choice(["Marié", "Célibataire", "Divorcé", "Veuf"]),
        "gender": random.choice(["Homme", "Femme"]),
        "number_of_children": random.randint(0, 5),
    }

# Génération d'une transaction
def generer_transaction(compte, isDormant = False):
    date_debut = datetime.datetime.strptime(compte['creation_date'], '%Y-%m-%dT%H:%M:%S') 
    date_fin = datetime.datetime.strptime(compte['closing_date'], '%Y-%m-%dT%H:%M:%S') if compte['closing_date'] != None else datetime.datetime(2024, 1, 1)
    date_fin_dormant = datetime.datetime(2022, 1, 1)
    transaction_date_dormant = generer_date(date_debut, date_fin_dormant)
    transaction_date = generer_date(date_debut, date_fin)
    return {
        "account_id": compte["account_id"],
        "transaction_id": random.randint(1, 100000),
        "transaction_date": transaction_date.isoformat() if isDormant == False else transaction_date_dormant.isoformat(),
        "transaction_amount": generer_montant(),
        "transaction_type": generer_type_transaction(),
        "transaction_city": generer_ville(),
        "transaction_agency": generer_agence(),
    }



# Génération des agios
def generer_agios(compte):
    debut_date_str = compte['creation_date'][0:10] 
    closing_date_str = datetime.datetime.now().strftime('%Y-%m-%d')
    # print(debut_date_str)
    # print(closing_date_str)
    result = find_end_of_months(debut_date_str, closing_date_str)
    balance = generer_montant()
    agios = []
    balance_after_agios = balance
    for date in result:
        balance_after_agios -= AGIOS_AMOUNT
        one_agios = {
            "account_id": compte['account_id'],
            "month": date.isoformat(),
            "balance_after_agios": balance_after_agios,
            "agios": AGIOS_AMOUNT
        }
        agios.append(one_agios)
    return agios 


if __name__ == "__main__":

    # Liste des comptes
    comptes = []
    comptes_dormant = []
    transactions = []
    agios = []

    # Génération des comptes
    for i in range(NB_COMPTES):
        comptes.append(generer_compte())

    # # Génération des transactions compte dormant
    for j in range(NB_COMPTES_DORMANTS):
        DUREE_SIMULATION_COMPTE_DORMANT = random.choice([1, 0, 1, 0, 0, 1, 2, 3])
        # On génère une transaction tous les mois
        for i in range(1, DUREE_SIMULATION_COMPTE_DORMANT * 12 + 1):
            transaction = generer_transaction(comptes[j], True)
            transactions.append(transaction)

    # # Génération des transactions
    for j in range(NB_COMPTES - NB_COMPTES_DORMANTS):
        # On génère une transaction tous les mois
        for i in range(1, DUREE_SIMULATION * 12 + 1):
            transaction = generer_transaction(comptes[j])
            transactions.append(transaction)

    # # Génération des agios
    for compte in comptes:
        if compte["closing_date"] is None:
            account_agios = generer_agios(compte)
            agios += account_agios

    # Enregistrement des données
    with open("accounts.json", "w", encoding='utf-8') as f:
        json.dump(comptes, f, indent=4, ensure_ascii=False)
    
    with open("transactions.json", "w", encoding='utf-8') as f:
        json.dump(transactions, f, indent=4, ensure_ascii=False)
    
    with open("agios.json", "w", encoding='utf-8') as f:
        json.dump(agios, f, indent=4, ensure_ascii=False)
