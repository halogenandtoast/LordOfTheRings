module LordOfTheRings.Quest.Cards.FightingInTheFields (fightingInTheFields) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (damageMinasTirith)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype FightingInTheFields = FightingInTheFields QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

fightingInTheFields :: QuestCard FightingInTheFields
fightingInTheFields = quest FightingInTheFields Cards.fightingInTheFields

{- | The Witch-king and encounter cards dealt to The Witch-king are immune to player card
effects: his shadow cards cannot be canceled. The players cannot defeat this stage unless
Aragorn is in play and The Witch-king is in the victory display. If the players defeat this
stage, they win the game (it is the last).
-}
instance HasModifiersFor FightingInTheFields where
  getModifiersFor (FightingInTheFields a) = when a.flipped do
    witchKing <- select (EnemyWithCardCode $ toCardCode Enemies.theWitchKingSorcerer)
    for_ witchKing \eid -> modified a eid [ImmuneToPlayerCardEffects, ShadowCannotBeCanceled]
    aragorn <- any (\h -> let Name t _ = h.name in t == "Aragorn") <$> (traverse getHero =<< select AnyHero)
    defeated <- getsGame (any ((== toCardCode Enemies.theWitchKingSorcerer) . toCardCode) . scenarioVictoryDisplay . toAttrs . (.scenario))
    unless (aragorn && defeated) $ modifySelf a [CannotBeDefeated]

-- | Forced: When an enemy attacks and destroys a character, deal 1 damage to Minas Tirith.
instance HasAbilities FightingInTheFields where
  getAbilities (FightingInTheFields a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After Nothing | a.flipped]

{- | When Revealed: The first player adds The Witch-king to the staging area (Sorcerer side
faceup). Then, he searches the encounter deck and discard pile for a Fell Beast and
attaches it to The Witch-king, if able. Each other player reveals an encounter card.
-}
instance RunMessage FightingInTheFields where
  runMessage msg q@(FightingInTheFields attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FightingInTheFields <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      captain <- getSetAside (CardWithCode $ toCardCode Enemies.theWitchKing)
      for_ captain \card -> do
        removeSetAsideFromGame [card]
        addToStaging card {ecCardCode = toCardCode Enemies.theWitchKingSorcerer}
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      witchKing <- select (EnemyWithCardCode $ toCardCode Enemies.theWitchKingSorcerer)
      let fellBeasts = filter ((== toCardCode Treacheries.fellBeast) . toCardCode) (scenario.encounterDeck <> scenario.encounterDiscard)
      for_ ((,) <$> headMay witchKing <*> headMay fellBeasts) \(eid, card) -> do
        attachFromEncounterDiscard card (EnemyTarget eid)
        shuffleEncounterDeck
      others <- drop 1 <$> getRemainingPlayers
      for_ others revealEncounterCardAs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      damageMinasTirith attrs 1
      pure q
    _ -> FightingInTheFields <$> runMessage msg attrs
