module LordOfTheRings.Hero.Cards.Haldan (haldan) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Haldan = Haldan HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

haldan :: HeroCard Haldan
haldan = hero Haldan Cards.haldan

-- | While the active location has an attachment on it, Haldan does not exhaust to quest.
instance HasModifiersFor Haldan where
  getModifiersFor (Haldan a) = do
    n <- activeLocationAttachments
    when (n > 0) $ modifySelf a [DoesNotExhaustToCommit]

-- | Response: When the active location is explored, draw 1 card for each attachment on
-- it. It is read while the location is still in play, its attachments on it.
instance HasAbilities Haldan where
  getAbilities (Haldan a) =
    [restricted a 1 (exists $ IsActiveLocation <> LocationWithAttachment) $ response $ ActiveLocationExploredWindow When]

instance RunMessage Haldan where
  runMessage msg h@(Haldan attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      attached <- for [lid | Window _ (ActiveLocationExplored lid) <- windows] (getAttachmentsOn . toTarget)
      drawCards attrs pid (sum $ map length attached)
      pure h
    _ -> Haldan <$> runMessage msg attrs
