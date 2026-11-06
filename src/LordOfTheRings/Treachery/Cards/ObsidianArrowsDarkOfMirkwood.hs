module LordOfTheRings.Treachery.Cards.ObsidianArrowsDarkOfMirkwood (obsidianArrowsDarkOfMirkwood) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ObsidianArrowsDarkOfMirkwood = ObsidianArrowsDarkOfMirkwood TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

obsidianArrowsDarkOfMirkwood :: TreacheryCard ObsidianArrowsDarkOfMirkwood
obsidianArrowsDarkOfMirkwood = treachery ObsidianArrowsDarkOfMirkwood Cards.obsidianArrowsDarkOfMirkwood

instance RunMessage ObsidianArrowsDarkOfMirkwood where
  runMessage msg t@(ObsidianArrowsDarkOfMirkwood a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pid <- getRevealer
      damageCharacter pid 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      damageCharacter pid 1
      pure t
    _ -> ObsidianArrowsDarkOfMirkwood <$> runMessage msg a
   where
    damageCharacter pid n = do
      characters <- charactersControlledBy pid
      chooseOrRunOneM pid do
        questionLabeled "obsidianArrowsDarkOfMirkwood.damage"
        causedBy a
        targets characters \c -> dealDamage a c n
