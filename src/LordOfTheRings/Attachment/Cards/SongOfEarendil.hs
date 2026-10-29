module LordOfTheRings.Attachment.Cards.SongOfEarendil (songOfEarendil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SongOfEarendil = SongOfEarendil AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit hero.
songOfEarendil :: AttachmentCard SongOfEarendil
songOfEarendil = attachment SongOfEarendil Cards.songOfEarendil & attachTo (AttachToHero $ HeroWithSphere Spirit)

-- | Response: After Song of Eärendil enters play, draw 1 card.
-- Response: After another player raises his threat, raise your threat by 1 to reduce
-- that player's threat by 1.
instance HasAbilities SongOfEarendil where
  getAbilities (SongOfEarendil a) =
    [ mkAbility a 1 $ response $ entersPlay a
    , mkAbility a 2 $ ResponseAbility (OtherPlayerRaisedThreatWindow After a.controller) (RaiseThreatCost 1)
    ]

instance RunMessage SongOfEarendil where
  runMessage msg s@(SongOfEarendil a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      for_ (take 1 [p | Window _ (ThreatRaised p) <- windows]) \p -> lowerThreatBy a p 1
      pure s
    _ -> SongOfEarendil <$> runMessage msg a
