module LordOfTheRings.Enemy.Cards.StalkingGoblin (stalkingGoblin) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype StalkingGoblin = StalkingGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stalkingGoblin :: EnemyCard StalkingGoblin
stalkingGoblin = enemy StalkingGoblin Cards.stalkingGoblin

instance HasAbilities StalkingGoblin where
  getAbilities (StalkingGoblin a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage StalkingGoblin where
  runMessage msg e@(StalkingGoblin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let found = find (cardMatches $ CardWithTitle "Goblin Sniper")
      case (found scenario.encounterDeck, found scenario.encounterDiscard) of
        (Just c, _) -> removeFromEncounterDeck c >> addToStaging c
        (_, Just c) -> removeFromEncounterDiscard c >> addToStaging c
        _ -> addToStaging =<< genEncounterCard Cards.goblinSniperDarkOfMirkwood
      shuffleEncounterDeck
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> StalkingGoblin <$> runMessage msg attrs
