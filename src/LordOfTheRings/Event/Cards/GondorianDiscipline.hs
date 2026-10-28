module LordOfTheRings.Event.Cards.GondorianDiscipline (gondorianDiscipline) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype GondorianDiscipline = GondorianDiscipline EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: Cancel up to 2 points of damage just dealt to a Gondor character.
gondorianDiscipline :: EventCard GondorianDiscipline
gondorianDiscipline =
  eventWith GondorianDiscipline Cards.gondorianDiscipline (windowL ?~ CharacterWithTraitDealtDamageWindow After Gondor)

instance RunMessage GondorianDiscipline where
  runMessage msg e@(GondorianDiscipline attrs) = case msg of
    -- the damage is on the character, its destruction not yet checked: the canceled
    -- points come off before it is
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (headMay [(t, n) | Window _ (DealtDamage t n) <- windows]) \(t, n) ->
        removeTokens attrs t Damage (min 2 n)
      pure e
    _ -> GondorianDiscipline <$> runMessage msg attrs
