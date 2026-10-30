module LordOfTheRings.Event.Cards.LastStand (lastStand) where

import LordOfTheRings.Card (cdAttack, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype LastStand = LastStand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a Warrior character is destroyed while defending against an enemy
-- attack, deal damage to the attacking enemy equal to that character's printed attack.
lastStand :: EventCard LastStand
lastStand = eventWith LastStand Cards.lastStand \a -> a & windowL ?~ DefenderWithTraitDestroyedWindow After Warrior

-- | The character's card comes with its destruction, in the same window.
instance RunMessage LastStand where
  runMessage msg e@(LastStand attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [(eid, t) | Window _ (CharacterDestroyedByAttack eid t _) <- windows]) \(eid, t) ->
        for_ (take 1 [c | Window _ (CharacterDestroyed t' c) <- windows, t' == t]) \c ->
          dealDamage attrs (EnemyTarget eid) (fromMaybe 0 $ cdAttack $ toCardDef c)
      pure e
    _ -> LastStand <$> runMessage msg attrs
