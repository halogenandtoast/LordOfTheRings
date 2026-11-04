module LordOfTheRings.Treachery.Cards.IllFate (illFate) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IllFate = IllFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of Breaking of the Fellowship's burden set.
illFate :: TreacheryCard IllFate
illFate = treachery IllFate Cards.illFate

-- | Counts as a Condition attachment with the text: "Forced: After a character you control
-- is destroyed, raise your threat by 2."
instance HasAbilities IllFate where
  getAbilities (IllFate a) = [mkAbility a 1 $ forced $ HostControllersCharacterDestroyedWindow After host | AttachedTo host@(HeroTarget _) <- [a.placement]]

-- | Peril. Surge. When Revealed: Attach to a hero you control.
instance RunMessage IllFate where
  runMessage msg t@(IllFate attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (HeroControlledBy $ PlayerWithId you)
      chooseOrRunOneM you do
        questionLabeled "illFate.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [h | AttachedTo (HeroTarget h) <- [attrs.placement]] \hid -> do
        you <- (.owner) <$> getHero hid
        for_ [() | Window _ (CharacterDestroyedUnder pid _) <- windows, pid == you] \_ -> raiseThreat you 2
      pure t
    _ -> IllFate <$> runMessage msg attrs
