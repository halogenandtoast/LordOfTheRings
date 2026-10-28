module LordOfTheRings.Treachery.Cards.TheMastersMalice (theMastersMalice) where

import LordOfTheRings.Card (cdSphere)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheMastersMalice = TheMastersMalice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMastersMalice :: TreacheryCard TheMastersMalice
theMastersMalice = treachery TheMastersMalice Cards.theMastersMalice

-- | When Revealed: Each player chooses 1 sphere of influence (leadership, lore, spirit, or
-- tactics). Each character a player controls that does not belong to his chosen sphere
-- takes 3 damage.
instance RunMessage TheMastersMalice where
  runMessage msg t@(TheMastersMalice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        characters <- traverse (\c -> (c,) . cdSphere <$> characterCardDef c) =<< charactersControlledBy p
        chooseOneM p do
          questionLabeled "theMastersMalice.chooseSphere"
          causedBy attrs
          for_ [(Leadership, "leadership"), (Lore, "lore"), (Spirit, "spirit"), (Tactics, "tactics")] \(sphere, key) ->
            labeled ("theMastersMalice." <> key)
              $ for_ [c | (c, s) <- characters, s /= Just sphere] \c -> dealDamage attrs c 3
      pure t
    _ -> TheMastersMalice <$> runMessage msg attrs
