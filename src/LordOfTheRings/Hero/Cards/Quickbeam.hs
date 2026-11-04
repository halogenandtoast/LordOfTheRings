module LordOfTheRings.Hero.Cards.Quickbeam (quickbeam) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Quickbeam = Quickbeam HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The errata makes Quickbeam unique (the first English printing lacked the unique
-- symbol; RingsDB marks the card errata'd), as heroes are on their card def. Its text is
-- unchanged.
quickbeam :: HeroCard Quickbeam
quickbeam = hero Quickbeam Cards.quickbeam

-- | Cannot have restricted attachments.
instance HasModifiersFor Quickbeam where
  getModifiersFor (Quickbeam a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Action: Deal 2 damage to Quickbeam to ready him. (Limit once per phase.)
instance HasAbilities Quickbeam where
  getAbilities (Quickbeam a) =
    [limited (PlayerLimit PerPhase 1) $ restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero) $ ActionAbility Free]

instance RunMessage Quickbeam where
  runMessage msg h@(Quickbeam attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealDamage attrs attrs 2
      ready attrs
      pure h
    _ -> Quickbeam <$> runMessage msg attrs
